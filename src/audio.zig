const c = @import("c.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn initAudio(allocator: Allocator) !*c.ma_engine {
    const engine = try allocator.create(c.ma_engine);
    errdefer allocator.destroy(engine);
    if (c.ma_engine_init(null, engine) != c.MA_SUCCESS) return error.FailedToInitEngine;
    return engine;
}

pub fn playFile(engine: *c.ma_engine, path: [*c]const u8) !void {
    if (c.ma_engine_play_sound(engine, path, null) != c.MA_SUCCESS) return error.FailedToPlaySound;
}
