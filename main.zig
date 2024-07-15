var global_tui_thingmabob_just_for_the_panic_handler_to_be_able_to_unset_it_if_it_has_to_lol: ?std.posix.termios = null;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const gpa_allocator = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(gpa_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    var iter = try std.process.argsWithAllocator(allocator);
    defer iter.deinit();

    _ = iter.next() orelse @panic("not a sane environment");

    const action_str = iter.next() orelse printHelp(null);
    if (isHelp(action_str)) {
        const actual_action_str = iter.next() orelse printHelp(null);
        const actual_action = std.meta.stringToEnum(Action, actual_action_str) orelse {
            std.log.err("Invalid action: '{s}'", .{actual_action_str});
            printHelp(null);
        };
        printHelp(actual_action);
    }

    const action = std.meta.stringToEnum(Action, action_str) orelse printHelp(null);

    switch (action) {
        .play => {
            const old = enterTui();
            global_tui_thingmabob_just_for_the_panic_handler_to_be_able_to_unset_it_if_it_has_to_lol = old;
            defer {
                global_tui_thingmabob_just_for_the_panic_handler_to_be_able_to_unset_it_if_it_has_to_lol = null;
                setTui(old);
            }

            // we want to manage strings ourselves so we dont need to dupe anything
            tl.taglib_set_string_management_enabled(0);

            var queue: std.ArrayListUnmanaged(PlayableSong) = .{};
            defer queue.deinit(allocator);

            var did_a_thing = false;
            while (iter.next()) |path| {
                did_a_thing = true;
                const file = std.fs.cwd().openFile(path, .{}) catch |err| {
                    die("failed to open file '{s}': {s}.", .{ path, @errorName(err) });
                };
                const buffer_len = file.getEndPos() catch |err| {
                    die("failed to get length of file '{s}': {s}", .{ path, @errorName(err) });
                };
                const buffer = try allocator.alloc(u8, buffer_len);
                const read_len = file.readAll(buffer) catch |err| {
                    die("failed to read file '{s}': {s}", .{ path, @errorName(err) });
                };
                if (read_len != buffer_len) {
                    die("failed to read file '{s}': expected {d} bytes, got {d}. Is this a regular file?", .{ path, buffer_len, read_len });
                }

                try queue.append(allocator, try PlayableSong.init(allocator, buffer));
            }

            try playQueue(allocator, queue.items);

            if (!did_a_thing) {
                std.log.err("expected a file argument", .{});
                printHelp(.play);
            }
        },
    }
}

const Song = struct {
    buffer: []const u8,
};

const SongInfo = struct {
    title: [*:0]const u8,
    artist: [*:0]const u8,
    album: [*:0]const u8,
    year: c_uint,
    pub fn init(buffer: []const u8) !SongInfo {
        const tag_iostream = tl.taglib_memory_iostream_new(buffer.ptr, @intCast(buffer.len));
        defer tl.taglib_iostream_free(tag_iostream);
        const tag_file = tl.taglib_file_new_iostream(tag_iostream);
        defer tl.taglib_file_free(tag_file);
        if (tl.taglib_file_is_valid(tag_file) != 1) return error.CouldNotGetSongInfo;
        const tag = tl.taglib_file_tag(tag_file);

        return .{
            .title = tl.taglib_tag_title(tag),
            .artist = tl.taglib_tag_artist(tag),
            .album = tl.taglib_tag_album(tag),
            .year = tl.taglib_tag_year(tag),
        };
    }
    pub fn deinit(si: SongInfo) void {
        tl.taglib_free(si.title);
        tl.taglib_free(si.artist);
        tl.taglib_free(si.album);
        comptime assert(@typeInfo(SongInfo).Struct.fields.len == 4);
    }
};

const PlayableSong = struct {
    info: SongInfo,
    buffer: []const u8,
    mp3: *c.drmp3,
    pub fn init(allocator: Allocator, buffer: []const u8) !PlayableSong {
        const mp3 = blk: {
            const mp3 = try allocator.alignedAlloc(u8, c.MAX_ALIGNMENT, c.sizeof_drmp3);
            if (c.drmp3_init_memory(@ptrCast(mp3.ptr), buffer.ptr, buffer.len, null) == c.DRMP3_FALSE) {
                die("failed to decode mp3 :/", .{});
            }
            break :blk mp3;
        };
        errdefer allocator.free(mp3);

        const mp3_ptr: *c.drmp3 = @ptrCast(mp3.ptr);
        return .{
            .buffer = buffer,
            .mp3 = mp3_ptr,
            .info = try SongInfo.init(buffer),
        };
    }
    pub fn deinit(ps: PlayableSong, allocator: Allocator) void {
        ps.info.deinit(allocator);
        const mp3_multipointer: [*]align(c.MAX_ALIGNMENT) u8 = @ptrCast(ps.mp3); 
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
        std.log.info("playing '{s}' by '{s}' from the album '{s}' ({d})", .{
            song.info.title, song.info.artist, song.info.album, song.info.year
        });
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
                const f = try std.fs.createFileAbsolute("/tmp/jomusic_queue", .{.truncate = true, .read = true});
                defer f.close();

                for (queue, 0..) |q, id| {
                    try f.writer().print("{d} #'{s}' from '{s}'\n", .{id, q.info.title, q.info.album});
                }

                var child = std.process.Child.init(&.{"nvim", "/tmp/jomusic_queue"}, allocator);
                _ = try child.spawnAndWait();

                try f.seekTo(0);
                const file_len = try f.getEndPos();
                const buf = try allocator.alloc(u8, file_len);
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
            \\      play        Play some music files in single shot mode.
            \\  
        ) catch {};
    } else switch (action.?) {
        .play => stderr.writeAll(
            \\Usage:
            \\  play <FILE 0> <FILE 1> ...<FILE N>             Play files in single-shot mode.
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

inline fn die(comptime fmt: []const u8, args: anytype) noreturn {
    std.io.getStdErr().writer().print(fmt, args) catch {};
    std.process.exit(1);
}

const Action = enum {
    play,
};

const FileType = enum {
    mp3,
};

const std = @import("std");
const tl = @import("taglib");
const c = @import("c.zig");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
