curr_line: []const u8,
next_time_ms: u64,
iter: std.mem.SplitIterator(u8, .scalar),

const SyncedLyrics = @This();

const NEXT_TIME_SENTINEL = std.math.maxInt(u64);

pub fn init(buf: []const u8) SyncedLyrics {
    var sl: SyncedLyrics = undefined;
    sl.iter = std.mem.splitScalar(u8, buf, '\n');
    sl.getNextLine();
    return sl;
}
pub fn getNextLine(sl: *SyncedLyrics) void {
    sl.curr_line = sl.iter.next().?;

    sl.next_time_ms = ms: {
        const next = sl.iter.peek() orelse break :ms NEXT_TIME_SENTINEL;
        break :ms extractMsTimeFromSynchronizedLyricLine(next) catch |err| switch (err) {
            error.ParseError => {
                sl.curr_line = "failed to parse lyrics file :/";
                break :ms NEXT_TIME_SENTINEL;
            },
        };
    };
}
fn extractMsTimeFromSynchronizedLyricLine(line: []const u8) error{ParseError}!u64 {
    const rbracket_idx = "[00:00.00".len;
    if (line.len == 0 or line.len < rbracket_idx) return error.ParseError;
    const time_str = line[1..rbracket_idx];

    var place_iter = std.mem.window(u8, time_str, 2, 3);
    const mins_str = place_iter.next() orelse return error.ParseError;
    const secs_str = place_iter.next() orelse return error.ParseError;
    const hundreths_str = place_iter.next() orelse return error.ParseError;

    const mins = std.fmt.parseInt(u32, mins_str, 10) catch return error.ParseError;
    const secs = std.fmt.parseInt(u32, secs_str, 10) catch return error.ParseError;
    const hundreths = std.fmt.parseInt(u32, hundreths_str, 10) catch return error.ParseError;
    const ms_per_hundreth = std.time.ms_per_s / 100;
    return (hundreths * ms_per_hundreth) +
        (secs * std.time.ms_per_s) +
        (mins * std.time.ms_per_min);
}

const std = @import("std");
