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
            const path = iter.next() orelse {
                std.log.err("expected a file argument.", .{});
                printHelp(.play);
            };

            const file = std.fs.cwd().openFile(path, .{}) catch |err| {
                die("failed to open file '{s}': {s}.", .{path, @errorName(err)});
            };
            const buffer_len = file.getEndPos() catch |err| {
                die("failed to get length of file '{s}': {s}", .{path, @errorName(err)});
            };
            const buffer = try allocator.alloc(u8, buffer_len);
            const read_len = file.readAll(buffer) catch |err| {
                die("failed to read file '{s}': {s}", .{path, @errorName(err)});
            };
            if (read_len != buffer_len) {
                die("failed to read file '{s}': expected {d} bytes, got {d}. Is this a regular file?", .{path, buffer_len, read_len});
            }

            const file_type = getFileTypeFromPath(path) orelse {
                die("invalid file extension for file '{s}'", .{path});
            };

            switch (file_type) {
                .mp3 => try playMp3(allocator, buffer),
            }
        },
    }
}

fn playMp3(allocator: Allocator, buffer: []const u8) !void {
    const mp3 = blk: {
        const mp3 = try allocator.alignedAlloc(u8, c.MAX_ALIGNMENT, c.sizeof_drmp3);
        if (c.drmp3_init_memory(@ptrCast(mp3.ptr), buffer.ptr, buffer.len, null) == c.DRMP3_FALSE) {
            die("failed to decode mp3 :/", .{});
        }
        break :blk mp3;
    };
    defer allocator.free(mp3);

    const mp3_ptr: *c.drmp3 = @ptrCast(mp3);

    const channels: u32 = @intCast(c.drmp3_channels(mp3_ptr));
    assert(channels == 1 or channels == 2);
    const sample_rate = c.drmp3_sample_rate(mp3_ptr);
    std.log.info("channels: {d}, sample rate: {d}", .{channels, sample_rate});
    const pulse_stream = initPulse(@intCast(channels), sample_rate);
    const frames_to_read: u32 = 1024;
    var buf: [1024*2]f32 = undefined;
    var i: u32 = 0;
    while (true) : (i += 1) {
        if (i % 3 == 0) {
            const stdout = std.io.getStdOut().writer();
            stdout.writeAll("\x1B[2K\x1B[1G") catch {};
            stdout.print("{}", .{
                fmtSecs(@divFloor(i*frames_to_read, sample_rate)),
            }) catch {};
        }
        const read = c.drmp3_read_pcm_frames_f32(mp3_ptr, frames_to_read, &buf);
        pulseWrite(pulse_stream, std.mem.sliceAsBytes(buf[0..read*channels]));
        if (read < frames_to_read) break;
    }
    pulseDrain(pulse_stream);
}

inline fn fmtSecs(secs: u64) FmtSecs {
    return .{.secs = secs};
}
const FmtSecs = struct {
    secs: u64,
    pub fn format(self: FmtSecs, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const mins = @divFloor(self.secs, std.time.s_per_min);
        const secs = self.secs % std.time.s_per_min;
        try writer.print("{d}:{d:0>2}", .{mins, secs});
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

const FRAME_BUFFER_LEN = 2048;

const std = @import("std");
const c = @import("c.zig");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
