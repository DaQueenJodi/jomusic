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
            const old = enterTui();
            global_tui_thingmabob_just_for_the_panic_handler_to_be_able_to_unset_it_if_it_has_to_lol = old;
            defer {
                global_tui_thingmabob_just_for_the_panic_handler_to_be_able_to_unset_it_if_it_has_to_lol = null;
                setTui(old);
            }

            var queue: std.ArrayListUnmanaged(PlayableSong) = .{};
            defer for (queue.items) |ps| ps.deinit(allocator);
            defer queue.deinit(allocator);

            var did_a_thing = false;

            var db = openDB();
            defer db.deinit();
            while (args_iter.next()) |id_str| {
                const id = std.fmt.parseInt(usize, id_str, 10) catch die("expected id argument to be a number, got: '{s}'", .{id_str});

                const ps = PlayableSong.initFromDB(&db, allocator, id) catch |err| switch (err) {
                    error.SongNotFound => {
                        std.log.info("song with ID '{d}' does not exist!", .{id});
                        break :blk;
                    },
                    error.OutOfMemory => |e| return e,
                };

                did_a_thing = true;
                try queue.append(allocator, ps);
            }
            if (!did_a_thing) {
                std.log.err("expected an argument", .{});
                printHelp(.play);
            }

            try playQueue(allocator, queue.items);
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

            printSongTable(arena, &stmt);
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
    }
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

fn printSongTable(arena: Allocator, mut_stmt: anytype) void {
    var diags: sql.Diagnostics = .{};
    const T = struct { id: u64, title: []const u8, artist: []const u8, album: ?[]const u8, year: c_uint };
    const songs = mut_stmt.all(T, arena, .{ .diags = &diags }, .{}) catch |err| {
        die("failed to iterate songs: {s}: {}", .{ @errorName(err), diags });
    };

    const max_id_len = calcColumnWidth(songs, "id");
    const max_title_len = calcColumnWidth(songs, "title");
    const max_album_len = calcColumnWidth(songs, "album");
    const max_artist_len = calcColumnWidth(songs, "artist");
    const max_year_len = calcColumnWidth(songs, "year");

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

            const written = printTruncatedSongData(writer, fmt, data, column_len);
            writer.writeByteNTimes(' ', column_len - written) catch {};
            writer.writeAll("│") catch {};
        }
        writer.writeByte('\n') catch {};
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

fn printTruncatedSongData(writer: anytype, comptime fmt: []const u8, data: anytype, max: u8) u8 {
    var buf: [MAX_SONG_COLUMN_LEN]u8 = undefined;
    const str, const overflew = blk: {
        const str = std.fmt.bufPrint(&buf, fmt, .{data}) catch |err| switch (err) {
            error.NoSpaceLeft => break :blk .{ &buf, true },
        };
        break :blk .{ str, str.len > max };
    };
    if (overflew) {
        assert(max > ELLIPSIS.len);
        const truncated = str[0 .. max - ELLIPSIS.len];
        const trimmed = std.mem.trimRight(u8, truncated, " ");
        writer.writeAll(trimmed) catch {};
        writer.writeAll(ELLIPSIS) catch {};
        const written = truncated.len + ELLIPSIS.len;
        writer.writeByteNTimes(' ', max - written) catch {};
        return max;
    } else {
        writer.writeAll(str) catch {};
        return @intCast(str.len);
    }
}

fn calcColumnWidth(songs: anytype, comptime field: []const u8) u8 {
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
                    if (data.len <= MAX_SONG_COLUMN_LEN) break :len @truncate(data.len);
                    // printTruncatedSongData trims the data before adding the elipses, so we need to replicate that here to be accurate
                    const trunc = data[0 .. MAX_SONG_COLUMN_LEN - ELLIPSIS.len];
                    const trimmed = std.mem.trimRight(u8, trunc, " ");
                    break :len @truncate(trimmed.len + ELLIPSIS.len);
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
        "CREATE TABLE IF NOT EXISTS songs (id INTEGER PRIMARY KEY, file_contents BLOB UNIQUE NOT NULL , title TEXT NOT NULL, album TEXT, artist TEXT NOT NULL, year INTEGER NOT NULL)",
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

const PlaySongExitStatus = enum {
    user_wants_next_song,
    user_wants_prev_song,
    user_wants_to_quit,
    user_wants_to_edit_queue,
    done_playing_normally,
};
// suspended_at_frame is only set if the exit status is .user_wants_prev_song or .user_wants_edit_queue
fn playSong(song: PlayableSong, seek_frames: u64, suspended_at_frame: *u64) !PlaySongExitStatus {
    if (seek_frames > 0) {
        if (c.drmp3_seek_to_pcm_frame(song.mp3, seek_frames) == c.DRMP3_FALSE) {
            std.log.err("failed to seek stream :(, playing from the beginning", .{});
        }
    }

    const channels: u8 = @intCast(c.drmp3_channels(song.mp3));
    const sample_rate = c.drmp3_sample_rate(song.mp3);

    const total_pcm_count = c.drmp3_get_pcm_frame_count(song.mp3);
    const length_in_secs = @divFloor(total_pcm_count, sample_rate);

    const pulse_stream = initPulse(@intCast(channels), sample_rate);
    defer c.pa_simple_free(pulse_stream);

    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    var paused = false;

    var i: u32 = 0;
    const exit: PlaySongExitStatus = play_loop: while (true) {
        var input_buf: [1]u8 = undefined;
        const len = try stdin.read(&input_buf);
        if (len > 0) {
            switch (input_buf[0]) {
                // next
                'n' => break :play_loop .user_wants_next_song,
                // previous
                'p' => break :play_loop .user_wants_prev_song,
                // quit
                'e' => break :play_loop .user_wants_to_quit,
                // queue
                'q' => break :play_loop .user_wants_to_edit_queue,
                // toggle pause
                ' ' => paused = !paused,
                else => {},
            }
        }
        if (paused) continue;
        if (i % sample_rate == 0) {
            // clear line
            stdout.writeAll("\x1B[2K\x1B[1G") catch {};
            stdout.print("{}/{}", .{
                fmtSecs(@divFloor(i, sample_rate)),
                fmtSecs(length_in_secs),
            }) catch {};
        }
        if (song.readNextFrame()) |frame| {
            pulseWrite(pulse_stream, std.mem.sliceAsBytes(frame[0..channels]));
        } else break :play_loop .done_playing_normally;

        i += 1;
    };
    pulseDrain(pulse_stream);
    // clear line
    stdout.writeAll("\x1B[2K\x1B[1G") catch {};
    stdout.writeAll(switch (exit) {
        .done_playing_normally => "(done)\n",
        .user_wants_next_song => "(skipped ↓)\n",
        .user_wants_prev_song => "(skipped ↑)\n",
        .user_wants_to_quit => "(exited)\n",
        .user_wants_to_edit_queue => "",
    }) catch {};

    switch (exit) {
        .user_wants_prev_song, .user_wants_to_edit_queue => suspended_at_frame.* = i,
        else => {},
    }

    return exit;
}

fn playQueue(allocator: Allocator, queue: []PlayableSong) !void {
    var curr_song_index: usize = 0;
    while (true) {
        const song = queue[curr_song_index];
        const si = song.info;
        if (si.album) |album| {
            std.log.info("playing '{s}' by '{s}' from the album '{s}' ({d})", .{ si.title, si.artist, album, si.year });
        } else {
            std.log.info("playing '{s}' ({d}) by '{s}'", .{ si.title, si.year, si.artist });
        }
        var suspended_at_frame: u64 = undefined;
        switch (try playSong(song, 0, &suspended_at_frame)) {
            .user_wants_to_quit => return,
            .user_wants_next_song => {
                curr_song_index = (curr_song_index + 1) % queue.len;
            },
            .done_playing_normally => {
                curr_song_index += 1;
                // exit if we just finished the last song
                if (curr_song_index == queue.len) return;
            },
            .user_wants_prev_song => {
                if (suspended_at_frame < 1_000) {
                    curr_song_index -|= 1;
                }
            },
            .user_wants_to_edit_queue => {
                const f = try std.fs.createFileAbsolute("/tmp/jomusic_queue", .{ .truncate = true, .read = true });
                defer f.close();

                for (queue, 0..) |q, id| {
                    if (q.info.album) |album| {
                        try f.writer().print("{d} #'{s}' by '{s}' from '{s}'\n", .{ id, q.info.title, album, q.info.artist });
                    } else {
                        try f.writer().print("{d} #'{s}' by '{s}'\n", .{ id, q.info.title, q.info.artist });
                    }
                }

                var child = std.process.Child.init(&.{ "nvim", "/tmp/jomusic_queue" }, allocator);
                _ = try child.spawnAndWait();

                try f.seekTo(0);
                const file_len = try f.getEndPos();
                const buf = try allocator.alloc(u8, file_len);
                defer allocator.free(buf);
                const real_len = try f.readAll(buf);
                // should always be true since its a regular file
                assert(real_len == file_len);

                const queue_copy = try allocator.dupe(PlayableSong, queue);
                defer allocator.free(queue_copy);

                var lines_iter = std.mem.splitScalar(u8, buf, '\n');
                var i: usize = 0;
                while (lines_iter.next()) |line| : (i += 1) {
                    if (line.len == 0) continue;
                    const space = std.mem.indexOfAny(u8, line, " #") orelse line.len;
                    const num_str = line[0..space];
                    const num = try std.fmt.parseInt(usize, num_str, 10);

                    queue[i] = queue[num];

                    if (queue.len == 0) {
                        std.log.info("queue is empty, exiting", .{});
                        return;
                    }
                    curr_song_index = @min(queue.len - 1, curr_song_index);
                }
            },
        }
    }
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
    _ = rate;
    // taken from https://www.freedesktop.org/software/pulseaudio/doxygen/simple.html#conn_sec
    const spec: c.pa_sample_spec = .{
        .format = c.PA_SAMPLE_FLOAT32,
        .channels = channels,
        .rate = 44100,
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

const MAX_SONG_COLUMN_LEN = 25;
const ELLIPSIS = "...";

const Action = enum {
    play,
    add,
    list,
    remove,
};

const FileType = enum {
    mp3,
};

const std = @import("std");
const builtin = @import("builtin");
const tl = @import("taglib");
const sql = @import("sqlite");
const c = @import("c.zig");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
