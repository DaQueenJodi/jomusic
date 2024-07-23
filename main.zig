var global_tui_thingmabob_just_for_the_panic_handler_to_be_able_to_unset_it_if_it_has_to_lol: ?std.posix.termios = null;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args_iter = try std.process.argsWithAllocator(allocator);
    defer args_iter.deinit();

    _ = args_iter.next() orelse @panic("not a sane environment");

    const action_str = args_iter.next() orelse printHelp(null);
    if (isHelp(action_str)) {
        const actual_action_str = args_iter.next() orelse printHelp(null);
        const actual_action = std.meta.stringToEnum(Action, actual_action_str) orelse {
            std.log.err("Invalid action: '{s}'", .{actual_action_str});
            printHelp(null);
        };
        printHelp(actual_action);
    }

    const action = std.meta.stringToEnum(Action, action_str) orelse printHelp(null);

    switch (action) {
        .play => blk: {
            // accumulate queue
            var queue = std.ArrayList(u64).init(allocator);
            defer queue.deinit();

            var db = openDB();
            defer db.deinit();
            while (args_iter.next()) |id_str| {
                const id = std.fmt.parseInt(usize, id_str, 10) catch die("expected id argument to be a number, got: '{s}'", .{id_str});
                try queue.append(id);
            }
            if (queue.items.len == 0) {
                std.log.err("expected at least one argument", .{});
                printHelp(.play);
            }

            const old = enterTui();
            global_tui_thingmabob_just_for_the_panic_handler_to_be_able_to_unset_it_if_it_has_to_lol = old;
            defer {
                global_tui_thingmabob_just_for_the_panic_handler_to_be_able_to_unset_it_if_it_has_to_lol = null;
                setTui(old);
            }

            var in_background: enum { no, queue, pager } = .no;
            var fetching_lyrics = false;

            var displaying_synced_lyrics = false;
            // only valid when `displaying_synced_lyrics` is true
            var synced_lyrics: SyncedLyrics = undefined;

            var state: enum { exit, paused, normal, next_song, next_song_normal, prev_song, wants_to_reload } = .normal;
            var curr_song_idx: u32 = 0;

            const stdout = std.io.getStdOut().writer();
            const stdin = std.io.getStdIn().reader();

            // only valid while `in_background` is true
            var running_process_id: std.posix.pid_t = undefined;

            // should only be accessed by the lyric fetching thread or when it's done working
            var fetching_arena_impl = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            const fetching_arena = fetching_arena_impl.allocator();
            // only valid while `fetching_lyrics` is true
            var fetching_state: FetchingState = undefined;

            // when len is 0, there is no warning
            var warning_to_display: std.BoundedArray(u8, WARNING_BUFFER_LEN) = .{};

            outer: while (true) {
                switch (state) {
                    .paused,
                    => unreachable,
                    .wants_to_reload => {
                        if (displaying_synced_lyrics) {
                            displaying_synced_lyrics = false;
                            allocator.free(synced_lyrics.iter.buffer);
                        }
                        stdout.writeAll("\x1B[2K\x1B[1G(switched songs)\n") catch {};
                        state = .normal;
                    },
                    .exit => {
                        if (displaying_synced_lyrics) {
                            displaying_synced_lyrics = false;
                            allocator.free(synced_lyrics.iter.buffer);
                        }
                        stdout.writeAll("\x1B[2K\x1B[1G(exitted)\n") catch {};
                        break;
                    },
                    .next_song => {
                        if (displaying_synced_lyrics) {
                            displaying_synced_lyrics = false;
                            allocator.free(synced_lyrics.iter.buffer);
                        }
                        curr_song_idx += 1;
                        state = .normal;
                        stdout.writeAll("\x1B[2K\x1B[1G(skipped)\n") catch {};
                    },
                    .next_song_normal => {
                        curr_song_idx += 1;
                        state = .normal;
                        stdout.writeAll("\x1B[2K\x1B[1G(done)\n") catch {};
                    },
                    .prev_song => {
                        curr_song_idx -= 1;
                        state = .normal;
                        stdout.writeAll("\x1B[2K\x1B[1G(skipped)\n") catch {};
                    },
                    .normal => {
                        const song_id = queue.items[curr_song_idx];
                        const song = PlayableSong.initFromDB(&db, allocator, song_id) catch |err| switch (err) {
                            error.SongNotFound => {
                                // TODO: make this not bad lol
                                if (in_background == .no and !fetching_lyrics) {
                                    std.log.info("song with ID '{d}' does not exist! skipping..", .{song_id});
                                }
                                break :blk;
                            },
                            error.OutOfMemory => |e| return e,
                        };
                        defer song.deinit(allocator);

                        if (in_background == .no and !fetching_lyrics) {
                            if (song.info.album) |album| {
                                std.log.info("playing '{s}' from '{s}' by '{s}'", .{ song.info.title, album, song.info.artist });
                            } else {
                                std.log.info("playing '{s}' by '{s}'", .{ song.info.title, song.info.artist });
                            }
                        }

                        const channels: u8 = @intCast(c.drmp3_channels(song.mp3));
                        const sample_rate = c.drmp3_sample_rate(song.mp3);

                        const total_pcm_count = c.drmp3_get_pcm_frame_count(song.mp3);
                        const length_in_secs = @divFloor(total_pcm_count, sample_rate);

                        const pulse_stream = initPulse(@intCast(channels), sample_rate);
                        defer c.pa_simple_free(pulse_stream);

                        var i: usize = 0;
                        inner: while (true) {
                            const input = inp: {
                                if (in_background != .no or fetching_lyrics) {
                                    break :inp 0;
                                }
                                const input = readByteOrZero(stdin);
                                if (warning_to_display.len > 0 and input != 0) {
                                    warning_to_display.len = 0;
                                    break :inp 0;
                                }
                                break :inp input;
                            };
                            switch (input) {
                                // just a stub for ergonomics
                                '\x00' => {},
                                // next
                                'n' => {
                                    state = .next_song;
                                    continue :outer;
                                },
                                // previous
                                'p' => {
                                    // if we're within the first 2 seconds, we go back to the previous song
                                    // otherwise, we'll just go to the beginning
                                    if (@divFloor(i, sample_rate) > 2) {
                                        i = 0;
                                        if (c.drmp3_seek_to_pcm_frame(song.mp3, 0) == c.DRMP3_FALSE) {
                                            warning_to_display.appendSlice("failed to seek to the start of the song :/") catch unreachable;
                                        }
                                    } else {
                                        state = .prev_song;
                                        continue :outer;
                                    }
                                },
                                // quit
                                'e' => {
                                    state = .exit;
                                    continue :outer;
                                },
                                // seeking
                                // left
                                'k', 'a' => {
                                    // seek back 5 seconds
                                    i -|= (5 * sample_rate);
                                    if (c.drmp3_seek_to_pcm_frame(song.mp3, i) == c.DRMP3_FALSE) {
                                        warning_to_display.appendSlice("failed to seek backwards :/") catch unreachable;
                                    }
                                },
                                // right
                                'j', 'd' => {
                                    i += (5 * sample_rate);
                                    i = @min(i, total_pcm_count);
                                    if (c.drmp3_seek_to_pcm_frame(song.mp3, i) == c.DRMP3_FALSE) {
                                        warning_to_display.appendSlice("failed to seek forwards :/") catch unreachable;
                                    }
                                },
                                // queue
                                'q' => {
                                    var arena_impl = std.heap.ArenaAllocator.init(std.heap.page_allocator);
                                    defer arena_impl.deinit();
                                    const arena = arena_impl.allocator();

                                    const pid_or_err = setupNvimQueue(arena, &db, queue.items, curr_song_idx);
                                    if (pid_or_err) |pid| {
                                        in_background = .queue;
                                        running_process_id = pid;
                                    } else |err| {
                                        const prefix = "failed to set up editable queue: ";
                                        inline for (@typeInfo(@TypeOf(err)).ErrorSet.?) |e| {
                                            comptime assert(e.name.len + prefix.len <= WARNING_BUFFER_LEN);
                                        }
                                        warning_to_display.writer().print("{s}{s}", .{ prefix, @errorName(err) }) catch unreachable;
                                    }
                                },
                                // clear screen
                                CTRL('l') => {
                                    std.io.getStdOut().writeAll("\x1B[2J") catch {};
                                    std.io.getStdOut().writeAll("\x1B[1;1H") catch {};
                                    if (song.info.album) |album| {
                                        std.log.info("playing '{s}' from '{s}' by '{s}'", .{ song.info.title, album, song.info.artist });
                                    } else {
                                        std.log.info("playing '{s}' by '{s}'", .{ song.info.title, song.info.artist });
                                    }
                                },
                                // lyrics
                                // 'l' => full page lyrics
                                // 'L' => inlined synchronized lyrics
                                inline 'l', 'L' => |l_flavor| {
                                    fetching_lyrics = true;
                                    displaying_synced_lyrics = l_flavor == 'L';
                                    fetching_state = .{
                                        .done = false,
                                        .fetched_lrc_id = null,
                                        .fetched = std.ArrayList(u8).init(fetching_arena),
                                        .result = undefined,
                                    };

                                    const maybe_lrc_id = db.one(
                                        u64,
                                        "SELECT lrc_id FROM lyrics WHERE song_id=?",
                                        .{},
                                        .{song_id},
                                    ) catch |err| {
                                        die("DB error: {s}", .{@errorName(err)});
                                    };

                                    const t = std.Thread.spawn(
                                        .{},
                                        fetchLyricsThreadFn,
                                        .{ &fetching_state, song.info, maybe_lrc_id, l_flavor == 'L' },
                                    ) catch |err| {
                                        const fmt = "failed to spawn lyric fetching thread: {s}";
                                        assertFitsInWarnings(fmt, @TypeOf(err));
                                        die(fmt, .{@errorName(err)});
                                    };
                                    t.detach();
                                    stdout.writeAll("\x1B[2K\x1B[1Gfetching lyrics..") catch {};
                                },
                                // toggle pause
                                ' ' => state = switch (state) {
                                    .paused => .normal,
                                    .normal => .paused,
                                    else => unreachable,
                                },
                                else => {},
                            }

                            if (!fetching_lyrics and in_background == .no and i % 50 == 0) {
                                // clear line
                                stdout.writeAll("\x1B[2K\x1B[1G") catch {};
                                if (displaying_synced_lyrics) {
                                    stdout.writeAll(synced_lyrics.curr_line) catch {};
                                    const milis = @divFloor(i * 1000, sample_rate);
                                    const target_ms = synced_lyrics.next_time_ms;
                                    if (milis >= target_ms) {
                                        synced_lyrics.getNextLine() catch |err| switch (err) {
                                            error.ParseError => {
                                                allocator.free(synced_lyrics.iter.buffer);
                                                warning_to_display.appendSlice("failed to parse line of lyrics file!") catch unreachable;
                                            },
                                        };
                                    }
                                } else {
                                    stdout.print("{}/{}", .{
                                        fmtSecs(@divFloor(i, sample_rate)),
                                        fmtSecs(length_in_secs),
                                    }) catch {};
                                    if (warning_to_display.len > 0) {
                                        stdout.print(" \x1B[31m{s}\x1B[m (press any key to dimiss..)", .{warning_to_display.slice()}) catch {};
                                    }
                                }
                            }
                            if (state != .paused) {
                                if (song.readNextFrame()) |frame| {
                                    pulseWrite(pulse_stream, std.mem.sliceAsBytes(frame[0..channels]));
                                    i += 1;
                                } else {
                                    state = .next_song;
                                    break :inner;
                                }
                            }

                            // poll if running process is done
                            if (in_background != .no) {
                                const ret = waitpid(running_process_id, null, std.os.linux.W.NOHANG);
                                if (ret > 0) {
                                    switch (in_background) {
                                        .queue => {
                                            const changed_songs = interpretNvimQueueFile(allocator, &queue, &curr_song_idx) catch |err| changed: {
                                                const fmt = "failed to parse queue file: {s}";
                                                assertFitsInWarnings(fmt, @TypeOf(err));
                                                warning_to_display.writer().print(fmt, .{@errorName(err)}) catch unreachable;
                                                break :changed false;
                                            };
                                            try std.fs.deleteFileAbsolute("/tmp/jomusic_queue");
                                            try std.fs.deleteFileAbsolute("/tmp/jomusic_others");

                                            if (changed_songs) {
                                                state = .wants_to_reload;
                                                in_background = .no;
                                                running_process_id = undefined;
                                                continue :outer;
                                            }
                                        },
                                        .pager => {},
                                        .no => unreachable,
                                    }
                                    in_background = .no;
                                    running_process_id = undefined;
                                }
                            }
                            // poll if thread is done fetching
                            // if it is, start the pager process
                            if (fetching_lyrics) {
                                const done = @atomicLoad(bool, &fetching_state.done, .monotonic);
                                if (done) {
                                    defer {
                                        fetching_state = undefined;
                                        fetching_lyrics = false;
                                        _ = fetching_arena_impl.reset(.free_all);
                                    }
                                    switch (fetching_state.result) {
                                        .out_of_memory => return error.OutOfMemory,
                                        inline .couldnt_parse_json,
                                        .fell_back_to_plain_lyrics,
                                        .couldnt_fetch,
                                        .unexpected_status_code,
                                        .song_not_found,
                                        .success,
                                        => |res| {
                                            const success = res == .success or res == .fell_back_to_plain_lyrics;
                                            if (success) {
                                                if (fetching_state.fetched_lrc_id) |lrc_id| {
                                                    db.exec(
                                                        "INSERT INTO lyrics (song_id, lrc_id) VALUES (?, ?)",
                                                        .{},
                                                        .{ song_id, lrc_id },
                                                    ) catch |err| {
                                                        die("DB error: {s}", .{@errorName(err)});
                                                    };
                                                }
                                            }

                                            const buf = switch (success) {
                                                true => fetching_state.fetched.items,
                                                false => {
                                                    displaying_synced_lyrics = false;
                                                    const fmt = "failed to fetch lyrics: {s}";
                                                    assertFitsInWarnings(fmt, @TypeOf(res));
                                                    warning_to_display.writer().print(fmt, .{@tagName(res)}) catch unreachable;
                                                    continue :inner;
                                                },
                                            };
                                            if (res == .fell_back_to_plain_lyrics) displaying_synced_lyrics = false;

                                            if (displaying_synced_lyrics) {
                                                const duped = try allocator.dupe(u8, buf);
                                                synced_lyrics = SyncedLyrics.init(duped) catch {
                                                    allocator.free(duped);
                                                    displaying_synced_lyrics = false;

                                                    warning_to_display.appendSlice("failed to parse lyrics file :/") catch unreachable;
                                                    continue :inner;
                                                };
                                            } else {
                                                const pid = try spawnPagerWithLyrics(buf);
                                                running_process_id = pid;
                                                in_background = .pager;
                                            }

                                        },
                                    }
                                }
                            }
                        }
                        pulseDrain(pulse_stream);
                    },
                }
            }
        },
        .add => {
            // we want to manage strings ourselves so we dont need to dupe anything
            tl.taglib_set_string_management_enabled(0);

            var arena_impl = std.heap.ArenaAllocator.init(allocator);
            defer arena_impl.deinit();
            const arena = arena_impl.allocator();
            const path = args_iter.next() orelse {
                std.log.err("expected file path argument", .{});
                printHelp(.add);
            };

            const f = std.fs.cwd().openFile(path, .{}) catch |err| {
                die("failed to open file '{s}': {s}", .{ path, @errorName(err) });
            };
            const f_len = f.getEndPos() catch |err| {
                die("failed to seek file '{s}': {s}. Is this a regular file?", .{ path, @errorName(err) });
            };

            const buf = try arena.alloc(u8, f_len);

            const real_len = f.readAll(buf) catch |err| {
                die("failed to read file '{s}': {s}", .{ path, @errorName(err) });
            };
            if (real_len != f_len) die("file length mismatch while reading '{s}'. Is it a regular file?", .{path});

            const found_metadata = MusicFileMetadata.initFromBuffer(buf) catch |err| switch (err) {
                error.NotAValidFile => die("file '{s}' does not seem to be a valid music file", .{path}),
            };
            defer found_metadata.deinit();

            var has_something_missing = false;
            inline for (@typeInfo(MusicFileMetadata).Struct.fields) |field| {
                if (@field(found_metadata, field.name) == null) has_something_missing = true;
            }

            const si = if (has_something_missing) blk: {
                break :blk haveUserFillInMissingFields(arena, found_metadata) catch |err| switch (err) {
                    error.OutOfMemory => |e| return e,
                };
            } else blk: {
                var si: SongInfo = undefined;
                inline for (@typeInfo(MusicFileMetadata).Struct.fields) |field| {
                    @field(si, field.name) = @field(found_metadata, field.name).?;
                }
                break :blk si;
            };

            var db = openDB();
            defer db.deinit();

            var diags: sql.Diagnostics = .{};
            const query = "INSERT INTO songs (title, file_contents, album, artist, year) VALUES (?,?,?,?,?)";
            db.exec(query, .{ .diags = &diags }, .{
                si.title,
                buf,
                si.album,
                si.artist,
                si.year,
            }) catch |err| {
                switch (err) {
                    error.SQLiteConstraintUnique => std.log.err("this file is already in the database, not adding!", .{}),
                    else => |e| die("failed to execute query '{s}': {s}: {}", .{ query, @errorName(e), diags }),
                }
            };
        },
        .list => {
            var arena_impl = std.heap.ArenaAllocator.init(allocator);
            defer arena_impl.deinit();
            const arena = arena_impl.allocator();
            var db = openDB();
            defer db.deinit();
            const songs_query = "SELECT id, title, artist, album, year FROM songs";
            var stmt = db.prepare(songs_query) catch |err| {
                die("failed to get list of songs from the database: {s}", .{@errorName(err)});
            };
            defer stmt.deinit();

            const arg = args_iter.next() orelse {
                try printSongTable(arena, &stmt);
                return;
            };
            if (std.mem.eql(u8, arg, "--fmt")) {
                const fmt = args_iter.next() orelse {
                    std.log.err("expected fmt argument!", .{});
                    printHelp(.list);
                };

                const T = struct { id: u64, title: []const u8, artist: []const u8, album: ?[]const u8, year: c_uint };
                var diags: sql.Diagnostics = .{};
                var iter = stmt.iterator(T, .{}) catch |err| {
                    die("failed to create song iterator: {s}", .{@errorName(err)});
                };
                const stdout = std.io.getStdOut().writer();

                while (iter.nextAlloc(arena, .{ .diags = &diags }) catch |err| {
                    die("failed to iterate songs table: {s}: {}", .{ @errorName(err), diags });
                }) |song| {
                    // syntax is like: "fooo {album} barrr {{aaaaaa}}"
                    var parse_state: enum { normal, maybe_fmt, maybe_closing_fmt, fmt } = .normal;
                    var fmt_start: usize = undefined;
                    for (fmt, 0..) |char, i| {
                        switch (char) {
                            '{' => switch (parse_state) {
                                .maybe_closing_fmt => die("invalid fmt string: estraneous '}}'", .{}),
                                .maybe_fmt => {
                                    stdout.writeByte('{') catch {};
                                    parse_state = .normal;
                                },
                                .normal => parse_state = .maybe_fmt,
                                .fmt => die("invalid fmt string: can't have a '{{' inside of a format argument!", .{}),
                            },
                            '}' => switch (parse_state) {
                                .normal => parse_state = .maybe_closing_fmt,
                                .maybe_closing_fmt => {
                                    parse_state = .normal;
                                    stdout.writeByte('}') catch {};
                                },
                                .maybe_fmt => die("invalid fmt string: empty format arg", .{}),
                                .fmt => {
                                    parse_state = .normal;
                                    const format_arg = fmt[fmt_start..i];
                                    const E = std.meta.FieldEnum(T);
                                    const field = std.meta.stringToEnum(E, format_arg) orelse {
                                        die("invalid fmt arg: '{s}', expected either 'album', 'id', 'artist', 'year', or 'title'", .{format_arg});
                                    };
                                    switch (field) {
                                        .album => stdout.writeAll(
                                            if (song.album) |album| album else "null",
                                        ) catch {},
                                        inline .id, .year => |t| stdout.print("{d}", .{@field(song, @tagName(t))}) catch {},
                                        inline .artist, .title => |t| stdout.writeAll(@field(song, @tagName(t))) catch {},
                                    }
                                },
                            },
                            else => switch (parse_state) {
                                .fmt => {},
                                .maybe_fmt => {
                                    fmt_start = i;
                                    parse_state = .fmt;
                                },
                                .maybe_closing_fmt => die("invalid fmt string: estraneous '}}'", .{}),
                                .normal => stdout.writeByte(char) catch {},
                            },
                        }
                    }
                    stdout.writeByte('\n') catch {};
                }
            } else {
                std.log.err("invalid argument: '{s}'", .{arg});
                printHelp(.list);
            }
        },
        .remove => {
            const id_str = args_iter.next() orelse {
                std.log.err("expected ID argument", .{});
                printHelp(.remove);
            };

            const id = std.fmt.parseInt(u64, id_str, 10) catch {
                std.log.err("expected ID to be a number, got '{s}'", .{id_str});
                printHelp(.remove);
            };

            var db = openDB();
            defer db.deinit();
            const query = "DELETE FROM songs AS s WHERE id=?";

            var diags: sql.Diagnostics = .{};
            db.exec(query, .{ .diags = &diags }, .{id}) catch |err| {
                die("failed to delete song with id: {d}: {s}: {}", .{ id, @errorName(err), diags });
            };
            const affected = db.rowsAffected();
            if (affected == 0) std.log.err("no song with id '{d}'", .{id});
        },
        .@"remove-lyrics" => {
            const id_str = args_iter.next() orelse {
                std.log.err("expected ID argument", .{});
                printHelp(.@"remove-lyrics");
            };
            const id = std.fmt.parseInt(u64, id_str, 10) catch {
                std.log.err("expected ID to be a number, got '{s}'", .{id_str});
                printHelp(.remove);
            };

            var db = openDB();
            defer db.deinit();
            const query = "DELETE FROM lyrics WHERE song_id=?";

            var diags: sql.Diagnostics = .{};
            db.exec(query, .{ .diags = &diags }, .{id}) catch |err| {
                die("failed to delete lyrics for song with id: {d}: {s}: {}", .{ id, @errorName(err), diags });
            };
            const affected = db.rowsAffected();
            if (affected == 0) std.log.info("no lyrics for song with id '{d}'", .{id});
        },
    }
}

/// returns true if the song thats playing has changed and the player should reload it
fn interpretNvimQueueFile(allocator: Allocator, queue: *std.ArrayList(u64), curr_queue_idx: *u32) error{
    CantOpenFile,
    CantReadFile,
    OutOfMemory,
    MultipleCurrent,
    NoCurrent,
    ParseError,
}!bool {
    const curr_song_id = queue.items[curr_queue_idx.*];

    const queue_file = std.fs.openFileAbsolute("/tmp/jomusic_queue", .{}) catch return error.CantOpenFile;
    const len = queue_file.getEndPos() catch return error.CantReadFile;
    const buf = try allocator.alloc(u8, len);
    defer allocator.free(buf);
    _ = queue_file.readAll(buf) catch return error.CantReadFile;

    queue.clearRetainingCapacity();
    var line_iter = std.mem.splitScalar(u8, buf, '\n');

    var changed_songs: bool = false;
    var saw_c = false;
    while (line_iter.next()) |line| {
        const significant_line = ln: {
            const idx = std.mem.indexOfScalar(u8, line, '#') orelse line.len;
            break :ln line[0..idx];
        };
        var space_iter = std.mem.tokenizeScalar(u8, significant_line, ' ');
        const id_str = space_iter.next() orelse continue;
        const id = std.fmt.parseInt(u64, id_str, 10) catch return error.ParseError;
        try queue.append(id);
        const maybe_current_marker = space_iter.next() orelse continue;
        if (std.mem.eql(u8, maybe_current_marker, "c")) {
            if (saw_c) {
                return error.MultipleCurrent;
            }
            saw_c = true;
            curr_queue_idx.* = @intCast(queue.items.len - 1);

            // if we switched the current song:
            if (curr_song_id != id) {
                changed_songs = true;
            }
        } else {
            return error.ParseError;
        }
    }
    if (!saw_c) {
        return error.NoCurrent;
    }
    return changed_songs;
}

fn setupNvimQueue(arena: Allocator, db: *sql.Db, queue: []u64, curr_queue_idx: usize) !std.posix.pid_t {
    {
        const queue_file = try std.fs.createFileAbsolute("/tmp/jomusic_queue", .{.truncate = true});
        defer queue_file.close();

        const queue_writer = queue_file.writer();

        for (queue[0..curr_queue_idx]) |curr_id| {
            try writeSongLineIntoQueueFile(false, arena, db, curr_id, queue_writer);
        }
        try writeSongLineIntoQueueFile(true, arena, db, queue[curr_queue_idx], queue_writer);
        for (queue[curr_queue_idx + 1 ..]) |curr_id| {
            try writeSongLineIntoQueueFile(false, arena, db, curr_id, queue_writer);
        }
    }

    {
        const all_songs_query = "SELECT id, title, artist, album FROM songs";
        var stmt = try db.prepare(all_songs_query);
        const T = struct { id: u64, title: []const u8, artist: []const u8, album: ?[]const u8 };
        var iter = stmt.iterator(T, .{}) catch |err| {
            die("failed to create song iterator: {s}", .{@errorName(err)});
        };

        const others_file = try std.fs.createFileAbsolute("/tmp/jomusic_others", .{.truncate = true});
        defer others_file.close();

        const others_writer = others_file.writer();

        while (try iter.nextAlloc(arena, .{})) |curr_song| {
            try others_writer.print("{d} #'{s}' by '{s}'", .{ curr_song.id, curr_song.title, curr_song.artist });
            if (curr_song.album) |album| try others_writer.print(" from '{s}'", .{album});
            try others_writer.writeByte('\n');
        }
    }

    const pid = try std.posix.fork();

    if (pid == 0) {
        // we are the child
        return std.posix.execvpeZ(
            "nvim",
            &.{ "nvim", "-O2", "/tmp/jomusic_queue", "/tmp/jomusic_others" },
            @ptrCast(std.os.environ),
        );
    } else {
        // we are the parent
        return pid;
    }
}

fn writeSongLineIntoQueueFile(comptime current: bool, arena: Allocator, db: *sql.Db, id: u64, writer: anytype) !void {
    const T = struct { artist: []const u8, album: ?[]const u8, title: []const u8 };
    var diags: sql.Diagnostics = .{};
    const maybe_curr_si = db.oneAlloc(
        T,
        arena,
        "SELECT artist, album, title FROM songs WHERE id=?",
        .{ .diags = &diags },
        .{id},
    ) catch |err| {
        die("failed to get song information from the database: {s}: {}", .{ @errorName(err), diags });
    };
    const curr_si = maybe_curr_si orelse {
        std.log.err("invalid id '{d}' in queue!", .{id});
        return;
    };

    try writer.print("{d}{s} # '{s}' by '{s}'", .{
        id,
        if (current) " c" else "",
        curr_si.title,
        curr_si.artist,
    });
    if (curr_si.album) |album| try writer.print(" from '{s}'", .{album});
    try writer.writeByte('\n');
}

fn readByteOrZero(reader: anytype) u8 {
    var buf: [1]u8 = undefined;
    const len = reader.read(&buf) catch 0;
    if (len == 0) return 0;
    return buf[0];
}

fn haveUserFillInMissingFields(arena: Allocator, found_metadata: MusicFileMetadata) !SongInfo {
    var si: SongInfo = undefined;
    const stdout = std.io.getStdOut().writer();
    stdout.writeAll("the file did not contain enough metadata, please fill in some fields below!\n") catch |err| {
        die("user input needed but writing to stdout failed: {s}", .{@errorName(err)});
    };
    inline for (@typeInfo(SongInfo).Struct.fields) |field| {
        if (@field(found_metadata, field.name)) |found| {
            @field(si, field.name) = found;
        } else {
            outer: while (true) {
                var arr = try std.ArrayList(u8).initCapacity(arena, 1024);
                const user_input_writer = arr.writer();

                const reader = std.io.getStdIn().reader();

                if (field.type == ?[]const u8) {
                    inner: while (true) {
                        stdout.writeAll("does this song have an album? y/No: ") catch |err| {
                            die("user input needed but writing to stdout failed: {s}", .{@errorName(err)});
                        };
                        reader.streamUntilDelimiter(user_input_writer, '\n', null) catch |err| {
                            die("failed to read user input: {s}", .{@errorName(err)});
                        };
                        const eql = std.ascii.eqlIgnoreCase;
                        const str = arr.items;
                        if (eql("yes", str) or eql("y", str)) {
                            break :inner;
                        } else if (eql("no", str) or eql("n", str)) {
                            si.album = null;
                            break :outer;
                        }
                        continue :inner;
                    }
                }

                stdout.writeAll("please enter the " ++ field.name ++ " for this song: ") catch |err| {
                    die("user input needed but writing to stdout failed: {s}", .{@errorName(err)});
                };
                reader.streamUntilDelimiter(user_input_writer, '\n', null) catch |err| {
                    die("failed to read user input: {s}", .{@errorName(err)});
                };

                if (arr.items.len == 0) continue :outer;
                const value = arr.items;
                switch (field.type) {
                    []const u8 => @field(si, field.name) = value,
                    c_uint => @field(si, field.name) = std.fmt.parseInt(c_uint, value, 10) catch |err| switch (err) {
                        error.Overflow => {
                            stdout.writeAll("year is too large!\n") catch |w_err| {
                                die("user input needed but writing to stdout failed: {s}", .{@errorName(w_err)});
                            };
                            continue :outer;
                        },
                        error.InvalidCharacter => {
                            stdout.writeAll("please enter a number for the year!\n") catch |w_err| {
                                die("user input needed but writing to stdout failed: {s}", .{@errorName(w_err)});
                            };
                            continue :outer;
                        },
                    },
                    else => unreachable,
                }
                break;
            }
        }
    }
    return si;
}

fn printSongTable(allocator: Allocator, mut_stmt: anytype) !void {
    var arena_impl = std.heap.ArenaAllocator.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var diags: sql.Diagnostics = .{};
    const T = struct { id: u64, title: []const u8, artist: []const u8, album: ?[]const u8, year: c_uint };
    const songs = mut_stmt.all(T, arena, .{ .diags = &diags }, .{}) catch |err| {
        die("failed to iterate songs: {s}: {}", .{ @errorName(err), diags });
    };

    const max_id_len = try calcColumnWidth(arena, songs, "id");
    const max_title_len = try calcColumnWidth(arena, songs, "title");
    const max_album_len = try calcColumnWidth(arena, songs, "album");
    const max_artist_len = try calcColumnWidth(arena, songs, "artist");
    const max_year_len = try calcColumnWidth(arena, songs, "year");
    _ = arena_impl.reset(.retain_capacity);

    const stdout = std.io.getStdOut();
    var bw = std.io.bufferedWriter(stdout.writer());
    defer bw.flush() catch {};
    const writer = bw.writer();

    const maxes = .{
        .id = max_id_len,
        .title = max_title_len,
        .album = max_album_len,
        .artist = max_artist_len,
        .year = max_year_len,
    };

    const fields = [_][]const u8{ "id", "title", "album", "artist", "year" };
    // top of table
    writer.writeAll("┌") catch {};
    inline for (fields, 0..) |field, i| {
        const column_len = @field(maxes, field);
        for (0..column_len) |_| writer.writeAll("─") catch {};
        if (comptime i != fields.len - 1) {
            writer.writeAll("┬") catch {};
        }
    }
    writer.writeAll("┐") catch {};
    writer.writeByte('\n') catch {};

    // print header
    writer.writeAll("│") catch {};
    inline for (fields) |field| {
        const column_len = @field(maxes, field);
        writer.print("{s}", .{field}) catch {};
        writer.writeByteNTimes(' ', column_len - field.len) catch {};
        writer.writeAll("│") catch {};
    }
    writer.writeByte('\n') catch {};

    const format_strings = .{
        .id = "{d}",
        .year = "{d}",
        .title = "{s}",
        .album = "{s}",
        .artist = "{s}",
    };

    for (songs) |song| {
        // write seperator between each row
        writer.writeAll("├") catch {};
        inline for (fields, 0..) |field, i| {
            const column_len = @field(maxes, field);
            for (0..column_len) |_| writer.writeAll("─") catch {};
            if (comptime i != fields.len - 1) {
                writer.writeAll("┼") catch {};
            }
        }
        writer.writeAll("┤") catch {};
        writer.writeByte('\n') catch {};
        writer.writeAll("│") catch {};
        inline for (fields) |field| {
            const column_len = @field(maxes, field);
            const fmt = @field(format_strings, field);
            const data_raw = @field(song, field);
            const data = if (@typeInfo(@TypeOf(data_raw)) == .Optional) blk: {
                break :blk data_raw orelse "null";
            } else data_raw;

            const written = try printTruncatedSongData(arena, writer, fmt, data, column_len);
            writer.writeByteNTimes(' ', column_len - written) catch {};
            writer.writeAll("│") catch {};
        }
        writer.writeByte('\n') catch {};

        _ = arena_impl.reset(.retain_capacity);
    }
    // bottom of the table
    writer.writeAll("└") catch {};
    inline for (fields, 0..) |field, i| {
        const column_len = @field(maxes, field);
        for (0..column_len) |_| writer.writeAll("─") catch {};
        if (comptime i != fields.len - 1) {
            writer.writeAll("┴") catch {};
        }
    }
    writer.writeAll("┘\n") catch {};
}

const MusicFileMetadata = struct {
    artist: ?[]u8,
    album: ?[]u8,
    title: ?[]u8,
    year: ?c_uint,
    pub fn initFromBuffer(buf: []const u8) !MusicFileMetadata {
        const tag_iostream = tl.taglib_memory_iostream_new(buf.ptr, @intCast(buf.len));
        defer tl.taglib_iostream_free(tag_iostream);
        const tag_file = tl.taglib_file_new_iostream(tag_iostream);
        defer tl.taglib_file_free(tag_file);
        if (tl.taglib_file_is_valid(tag_file) != 1) return error.NotAValidFile;
        const tag = tl.taglib_file_tag(tag_file);

        const title = std.mem.span(tl.taglib_tag_title(tag));
        const artist = std.mem.span(tl.taglib_tag_artist(tag));
        const album = std.mem.span(tl.taglib_tag_album(tag));
        const year = tl.taglib_tag_year(tag);
        return .{
            .artist = if (artist.len > 0) artist else null,
            .album = if (album.len > 0) album else null,
            .title = if (album.len > 0) title else null,
            .year = if (year > 0) year else null,
        };
    }
    pub fn deinit(mfm: MusicFileMetadata) void {
        if (mfm.artist) |artist| tl.taglib_free(artist.ptr);
        if (mfm.album) |album| tl.taglib_free(album.ptr);
        if (mfm.title) |title| tl.taglib_free(title.ptr);
    }
};

fn printTruncatedSongData(arena: Allocator, writer: anytype, comptime fmt: []const u8, data: anytype, max: u8) !u8 {
    switch (@TypeOf(data)) {
        u64, c_uint => {
            var buf: [MAX_SONG_COLUMN_LEN]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{d}", .{data}) catch &buf;
            if (str.len > max) {
                writer.writeAll(str[0 .. max - ELLIPSIS.len]) catch {};
                writer.writeAll(ELLIPSIS) catch {};
                return max;
            }
            writer.writeAll(str) catch {};
            return @intCast(str.len);
        },
        ?[]const u8 => {
            return try printTruncatedSongData(arena, writer, fmt, data.?, max);
        },
        []const u8 => {
            const dwd = try DisplayWidth.DisplayWidthData.init(arena);
            defer dwd.deinit();

            const dw = DisplayWidth{ .data = &dwd };

            const gd = try grapheme.GraphemeData.init(arena);
            defer gd.deinit();

            var grapheme_iter = grapheme.Iterator.init(data, &gd);

            const full_width = dw.strWidth(data);
            if (full_width <= max) {
                writer.writeAll(data) catch {};
                return @intCast(full_width);
            }

            var last_non_overflowing_non_whitespace_end: usize = 0;
            var last_non_overflowing_non_whitespace_width: usize = 0;
            while (grapheme_iter.next()) |gc| {
                const full_off = gc.offset + gc.len;
                const str = data[0..full_off];
                const width = dw.strWidth(str);
                if (width <= max - ELLIPSIS.len) {
                    if (!std.mem.eql(u8, gc.bytes(str), " ")) {
                        last_non_overflowing_non_whitespace_end = full_off;
                        last_non_overflowing_non_whitespace_width = width;
                    }
                }
                if (width > max) {
                    break;
                }
            }
            writer.writeAll(data[0..last_non_overflowing_non_whitespace_end]) catch {};
            writer.writeAll(ELLIPSIS) catch {};
            return @intCast(last_non_overflowing_non_whitespace_width + ELLIPSIS.len);
        },
        else => unreachable,
    }
}

fn calcColumnWidth(arena: Allocator, songs: anytype, comptime field: []const u8) !u8 {
    // the column needs to be at least as big as the name of the column
    var max: u8 = field.len;
    for (songs) |song| {
        const len: u8 = len: {
            const raw_data = @field(song, field);
            const data = if (@typeInfo(@TypeOf(raw_data)) == .Optional) data: {
                break :data raw_data orelse break :len "null".len;
            } else raw_data;
            switch (@TypeOf(data)) {
                []const u8 => {
                    const dwd = try DisplayWidth.DisplayWidthData.init(arena);
                    defer dwd.deinit();

                    const dw = DisplayWidth{ .data = &dwd };

                    const len = dw.strWidth(data);
                    break :len @truncate(len);
                },
                u64, c_uint => break :len @truncate(std.fmt.count("{d}", .{data})),
                else => unreachable,
            }
        };
        if (len > max) max = len;
    }
    return @min(max, MAX_SONG_COLUMN_LEN);
}

fn openDB() sql.Db {
    var diags: sql.Diagnostics = .{};
    var db = sql.Db.init(.{
        .mode = .{ .File = "jomusic.db" },
        .open_flags = .{ .write = true, .create = true },
        .diags = &diags,
    }) catch |err| {
        die("failed to open jomusic database: {s}: {}", .{ @errorName(err), diags });
    };
    errdefer db.deinit();

    if (sql.c.sqlite3_extended_result_codes(db.db, 1) != sql.c.SQLITE_OK) {
        die("failed to enable extended result codes", .{});
    }
    setupDBIfNeeded(&db);

    return db;
}

fn setupDBIfNeeded(db: *sql.Db) void {
    var diags: sql.Diagnostics = .{};
    db.exec(
        "CREATE TABLE IF NOT EXISTS songs (id INTEGER PRIMARY KEY, file_contents BLOB UNIQUE NOT NULL, title TEXT NOT NULL, album TEXT, artist TEXT NOT NULL, year INTEGER NOT NULL)",
        .{ .diags = &diags },
        .{},
    ) catch |err| {
        die("failed to set up database: {s}: {}", .{ @errorName(err), diags });
    };
    db.exec(
        "CREATE TABLE IF NOT EXISTS lyrics (id INTEGER PRIMARY KEY, lrc_id INTEGER NOT NULL, song_id INTEGER UNIQUE NOT NULL)",
        .{ .diags = &diags },
        .{},
    ) catch |err| {
        die("failed to set up database: {s}: {}", .{ @errorName(err), diags });
    };
}

const Song = struct {
    id: u64,
    pub fn getSongInfo(s: Song, allocator: Allocator, db: sql.Db) SongInfo {
        const query = "SELECT title, album, artist, year FROM songs AS s WHERE s.id = ?";

        var diags: sql.Diagnostics = .{};
        const maybe_song_info = db.oneAlloc(SongInfo, allocator, query, .{s.id}, .{ .diags = &diags }) catch |err| {
            die("failed to get song info for song with ID '{d}': {s}: {}", .{ s.id, @errorName(err), diags });
        };

        return maybe_song_info orelse {
            die("no song info found for song with ID: '{d}'", .{s.id});
        };
    }
};

const SongInfo = struct {
    title: []const u8,
    artist: []const u8,
    album: ?[]const u8,
    year: c_uint,
};

const PlayableSong = struct {
    info: SongInfo,
    buffer: []const u8,
    mp3: *c.drmp3,
    pub fn initFromDB(db: *sql.Db, allocator: Allocator, id: u64) !PlayableSong {
        const si = getSongRowFromDb(db, allocator, id) orelse return error.SongNotFound;
        const mp3 = blk: {
            const mp3 = try allocator.alignedAlloc(u8, c.MAX_ALIGNMENT, c.sizeof_drmp3);
            if (c.drmp3_init_memory(@ptrCast(mp3.ptr), si.file_contents.ptr, si.file_contents.len, null) == c.DRMP3_FALSE) {
                die("failed to decode mp3 :/", .{});
            }
            break :blk mp3;
        };
        errdefer allocator.free(mp3);

        const mp3_ptr: *c.drmp3 = @ptrCast(mp3.ptr);
        return .{
            .buffer = si.file_contents,
            .mp3 = mp3_ptr,
            .info = .{
                .title = si.title,
                .artist = si.artist,
                .album = si.album,
                .year = si.year,
            },
        };
    }
    pub fn deinit(ps: PlayableSong, allocator: Allocator) void {
        allocator.free(ps.buffer);

        allocator.free(ps.info.artist);
        allocator.free(ps.info.title);
        if (ps.info.album) |album| allocator.free(album);

        const mp3_multipointer: [*]align(c.MAX_ALIGNMENT) u8 = @alignCast(@ptrCast(ps.mp3));
        allocator.free(mp3_multipointer[0..c.sizeof_drmp3]);
    }
    pub fn readNextFrame(ps: PlayableSong) ?[2]f32 {
        var buf: [2]f32 = undefined;
        const read = c.drmp3_read_pcm_frames_f32(ps.mp3, 1, &buf);
        if (read == 0) return null;
        assert(read == 1);
        return buf;
    }
};

const SongRow = struct { file_contents: []const u8, title: []const u8, album: ?[]const u8, artist: []const u8, year: c_uint };
fn getSongRowFromDb(db: *sql.Db, allocator: Allocator, id: u64) ?SongRow {
    const query = "SELECT file_contents, title, album, artist, year FROM songs WHERE id=?";
    var diags: sql.Diagnostics = .{};
    return db.oneAlloc(SongRow, allocator, query, .{ .diags = &diags }, .{id}) catch |err| {
        die("failed to retrieve song row from database: {s}: {}", .{ @errorName(err), diags });
    };
}

inline fn fmtSecs(secs: u64) FmtSecs {
    return .{ .secs = secs };
}
const FmtSecs = struct {
    secs: u64,
    pub fn format(self: FmtSecs, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const mins = @divFloor(self.secs, std.time.s_per_min);
        const secs = self.secs % std.time.s_per_min;
        try writer.print("{d}:{d:0>2}", .{ mins, secs });
    }
};

inline fn pulseWrite(pulse_stream: *c.pa_simple, frames: []const u8) void {
    var err: c_int = undefined;
    if (c.pa_simple_write(pulse_stream, frames.ptr, frames.len, &err) < 0) {
        die("failed to write to pulse stream: {s}", .{c.pa_strerror(err)});
    }
}

inline fn pulseDrain(pulse_stream: *c.pa_simple) void {
    var err: c_int = undefined;
    if (c.pa_simple_drain(pulse_stream, &err) < 0) {
        die("failed to drain pulse stream: {s}", .{c.pa_strerror(err)});
    }
}

fn initPulse(channels: u8, rate: u32) *c.pa_simple {
    // taken from https://www.freedesktop.org/software/pulseaudio/doxygen/simple.html#conn_sec
    const spec: c.pa_sample_spec = .{
        .format = c.PA_SAMPLE_FLOAT32,
        .channels = channels,
        .rate = rate,
    };
    var err: c_int = undefined;
    return c.pa_simple_new(null, "jomusic", c.PA_STREAM_PLAYBACK, null, "Music", &spec, null, null, &err) orelse {
        die("could not initialize pulseaudio: {s}", .{c.pa_strerror(err)});
    };
}

inline fn getFileTypeFromPath(path: []const u8) ?FileType {
    inline for (@typeInfo(FileType).Enum.fields) |field| {
        if (std.mem.endsWith(u8, path, "." ++ field.name)) return @field(FileType, field.name);
    }
    return null;
}

inline fn isHelp(str: []const u8) bool {
    return std.mem.eql(u8, str, "--help") or std.mem.eql(u8, str, "-h");
}

fn printHelp(action: ?Action) noreturn {
    const stderr = std.io.getStdErr().writer();
    if (action == null) {
        stderr.writeAll(
            \\Usage:
            \\  --help, -h              Display this help message and exit. 
            \\  --help, -h <ACTION>     Display a help message for a specific action (defined below) and exit.
            \\
            \\  Use -h or --help with any subcommand to get information about that subcommand
            \\  Actions:
            \\      play        Play some music from the database
            \\      add         Add music to the database
            \\      list        List music in the database
            \\      remove      Remove a song from the database
            \\      remove-lyrics   Remove lyrics from the database
            \\  
        ) catch {};
    } else switch (action.?) {
        .play => stderr.writeAll(
            \\Usage:
            \\  play <ID 1> <ID 2> <ID N>     Play songs from the database using an ID.
            \\
        ) catch {},
        .add => stderr.writeAll(
            \\Usage:
            \\  add <FILE>  Add a file to the database with metadata.
            \\
        ) catch {},
        .list => stderr.writeAll(
            \\Usage:
            \\  list        list songs in the database.
            \\
        ) catch {},
        .remove => stderr.writeAll(
            \\Usage:
            \\  remove <ID>     remove the song with that ID from the database.
            \\
        ) catch {},
        .@"remove-lyrics" => stderr.writeAll(
            \\Usage:
            \\  remove-lyrics <ID>      remove the lyrics for the song corresponding to that ID from the database
            \\
        ) catch {},
    }

    std.process.exit(1);
}

fn enterTui() std.posix.termios {
    const tty_file = std.fs.cwd().openFile("/dev/tty", .{}) catch |err| die("could not open terminal: {s}", .{@errorName(err)});
    defer tty_file.close();
    const old = std.posix.tcgetattr(tty_file.handle) catch |err| die("could not get termios attrs: {s}", .{@errorName(err)});
    var new = old;
    new.lflag.ECHO = false;
    new.lflag.ICANON = false;
    const V = std.os.linux.V;
    new.cc[@intFromEnum(V.MIN)] = 0;
    new.cc[@intFromEnum(V.TIME)] = 0;
    setTui(new);
    return old;
}

fn setTui(new: std.posix.termios) void {
    const tty_file = std.fs.cwd().openFile("/dev/tty", .{}) catch |err| die("could not open terminal: {s}", .{@errorName(err)});
    defer tty_file.close();
    std.posix.tcsetattr(tty_file.handle, .NOW, new) catch |err| die("could not set termios attrs: {s}", .{@errorName(err)});
}

pub fn panic(msg: []const u8, error_return_trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    if (global_tui_thingmabob_just_for_the_panic_handler_to_be_able_to_unset_it_if_it_has_to_lol) |t| setTui(t);
    @call(.always_inline, std.builtin.default_panic, .{ msg, error_return_trace, ret_addr });
}

pub const std_options = .{
    .logFn = logThatExcludesSQL,
};

pub fn logThatExcludesSQL(comptime level: std.log.Level, comptime scope: @TypeOf(.EnumLiteral), comptime format: []const u8, args: anytype) void {
    if (scope == .sqlite) return;
    std.log.defaultLog(level, scope, format, args);
}

inline fn die(comptime fmt: []const u8, args: anytype) noreturn {
    switch (builtin.mode) {
        .Debug => std.debug.panic(fmt, args),
        else => {
            if (global_tui_thingmabob_just_for_the_panic_handler_to_be_able_to_unset_it_if_it_has_to_lol) |t| setTui(t);
            std.log.err(fmt, args);
            std.process.exit(1);
        },
    }
}

// same as std.os.linux but status is allowed to be NULL
fn waitpid(pid: std.os.linux.pid_t, status: ?*u32, flags: u32) usize {
    return std.os.linux.syscall4(.wait4, @as(usize, @bitCast(@as(isize, pid))), @intFromPtr(status), flags, 0);
}

fn spawnPagerWithLyrics(text: []const u8) !std.posix.pid_t {
    const read_pipe, const write_pipe = try std.posix.pipe2(.{});

    const fork_pid = try std.posix.fork();
    if (fork_pid == 0) {
        // we are the child
        std.posix.close(write_pipe);
        try std.posix.dup2(read_pipe, std.posix.STDIN_FILENO);
        return std.posix.execvpeZ("less", &.{"less"}, @ptrCast(std.os.environ.ptr));
    } else {
        std.posix.close(read_pipe);
        // we are the parent
        const f: std.fs.File = .{ .handle = write_pipe };
        try f.writeAll(text);
        f.close();
        return fork_pid;
    }
}

const FetchingState = struct {
    done: bool,
    fetched_lrc_id: ?u64,
    fetched: std.ArrayList(u8),
    result: enum { success, out_of_memory, couldnt_fetch, song_not_found, couldnt_parse_json, fell_back_to_plain_lyrics, unexpected_status_code },
};
fn fetchLyricsThreadFn(state: *FetchingState, si: SongInfo, maybe_lrc_id: ?u64, comptime synced_lyrics: bool) void {
    defer @atomicStore(bool, &state.done, true, .monotonic);

    const arena = state.fetched.allocator;
    var client = std.http.Client{
        .allocator = arena,
    };

    const path, const query = if (maybe_lrc_id) |lrc_id| blk: {
        const path = std.fmt.allocPrint(arena, "/api/get/{d}", .{lrc_id}) catch {
            state.result = .out_of_memory;
            return;
        };
        break :blk .{ path, "" };
    } else blk: {
        const query = switch (si.album != null) {
            true => std.fmt.allocPrint(arena, "track_name={s}&artist_name={s}&album_name={s}", .{ si.title, si.artist, si.album.? }) catch {
                state.result = .out_of_memory;
                return;
            },
            false => std.fmt.allocPrint(arena, "track_name={s}&artist_name={s}", .{ si.title, si.artist }) catch {
                state.result = .out_of_memory;
                return;
            },
        };
        break :blk .{ "/api/search", query };
    };

    const uri: std.Uri = .{
        .scheme = "https",
        .host = .{ .percent_encoded = "lrclib.net" },
        .path = .{ .percent_encoded = path },
        .query = .{ .raw = query },
    };

    const res = client.fetch(.{
        .response_storage = .{
            .dynamic = &state.fetched,
        },
        .location = .{
            .uri = uri,
        },
        .headers = .{
            // as encouraged by https://lrclib.net/docs
            .user_agent = .{ .override = "jomusic 0.1.0 (https://github.com/daqueenjodi/jomusic" },
        },
    }) catch {
        state.result = .couldnt_fetch;
        return;
    };
    switch (res.status) {
        .ok => {},
        .not_found => {
            state.result = .song_not_found;
            return;
        },
        else => {
            state.result = .unexpected_status_code;
            return;
        },
    }

    if (maybe_lrc_id == null) {
        const JsonT = switch (synced_lyrics) {
            true => struct {
                id: u64,
                syncedLyrics: ?[]const u8,
                plainLyrics: []const u8,
            },
            false => struct {
                id: u64,
                plainLyrics: []const u8,
            },
        };
        const fetched = std.json.parseFromSliceLeaky([]JsonT, arena, state.fetched.items, .{
            .ignore_unknown_fields = true,
            .allocate = .alloc_always,
        }) catch |err| {
            state.result = switch (err) {
                error.OutOfMemory => .out_of_memory,
                error.UnknownField => unreachable,
                error.MissingField => .couldnt_fetch,
                else => .couldnt_parse_json,
            };
            return;
        };

        const song = fetched[0];
        
        var fell_back = false;
        const lyrics = blk: {
            if (synced_lyrics) {
                break :blk song.syncedLyrics orelse {
                    fell_back = true;
                    break :blk song.plainLyrics;
                };
            } else break :blk song.plainLyrics;
        };
        state.fetched.clearRetainingCapacity();
        state.fetched.appendSlice(lyrics) catch {
            state.result = .out_of_memory;
            return;
        };
        state.fetched_lrc_id = song.id;

        if (fell_back) {
            state.result = .fell_back_to_plain_lyrics;
            return;
        }

    } else {
        state.fetched_lrc_id = null;

        const JsonT = switch (synced_lyrics) {
            true => struct {
                syncedLyrics: ?[]const u8,
                plainLyrics: []const u8,
            },
            false => struct {
                plainLyrics: []const u8,
            },
        };
        const song = std.json.parseFromSliceLeaky(JsonT, arena, state.fetched.items, .{
            .ignore_unknown_fields = true,
            .allocate = .alloc_always,
        }) catch |err| {
            state.result = switch (err) {
                error.OutOfMemory => .out_of_memory,
                error.UnknownField => unreachable,
                error.MissingField => .couldnt_fetch,
                else => .couldnt_parse_json,
            };
            return;
        };
        var fell_back = false;
        const lyrics = blk: {
            if (synced_lyrics) {
                break :blk song.syncedLyrics orelse {
                    fell_back = true;
                    break :blk song.plainLyrics;
                };
            } else break :blk song.plainLyrics;
        };
        state.fetched.clearRetainingCapacity();
        state.fetched.appendSlice(lyrics) catch {
            state.result = .out_of_memory;
            return;
        };
        if (fell_back) {
            state.result = .fell_back_to_plain_lyrics;
            return;
        }
    }

    state.result = .success;
}

const Action = enum {
    play,
    add,
    list,
    remove,
    @"remove-lyrics",
};

const FileType = enum {
    mp3,
};

fn assertFitsInWarnings(comptime fmt: []const u8, comptime T: type) void {
    const info = @typeInfo(T);
    const fields = switch (info) {
        .ErrorSet => |es| es orelse return,
        .Enum => |e| e.fields,
        else => unreachable,
    };
    inline for (fields) |field| {
        comptime assert(std.fmt.count(fmt, .{field.name}) <= WARNING_BUFFER_LEN);
    }
}

fn CTRL(char: u8) u8 {
    return char - 'a' + 1;
}

const WARNING_BUFFER_LEN = 1024;
const MAX_SONG_COLUMN_LEN = 35;
const ELLIPSIS = "...";

const std = @import("std");
const builtin = @import("builtin");
const tl = @import("taglib");
const sql = @import("sqlite");
const grapheme = @import("zg-grapheme");
const DisplayWidth = @import("zg-DisplayWidth");
const c = @import("c.zig");
const SyncedLyrics = @import("SyncedLyrics.zig");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
