const std = @import("std");
const c = @import("c.zig");
const Metadata = @import("Metadata.zig");
const audio = @import("audio.zig");

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    _ = allocator;
   //var metadata = try Metadata.init(allocator, "welp.mp3");
   //try metadata.prompt_missing(allocator);
   //const config = try Metadata.MetadataConfig.fromMetadata(metadata, "welp.mp3");
   //std.debug.print("{s}\n", .{try config.toJson(allocator)});
    var engine: c.ma_engine = undefined;
    _ = c.ma_engine_init(null, &engine);
    _ = c.ma_engine_play_sound(&engine, "welp.mp3", null);
    while (true) { std.time.sleep(std.time.ns_per_s); }
}

