const std = @import("std");
const Metadata = @import("metadata.zig").Metadata;
const Allocator = std.mem.Allocator;

const Entry = struct {
    metadata: Metadata,
    file: []u8
};

pub const Database = struct {
    songs: std.ArrayList(Entry),
    const Self = @This();
    pub fn init(allocator: Allocator) !Self {
        return Self {
            .songs = std.ArrayList(Song).init(allocator)
        };
    }
    pub fn deinit(self: *Self) void {
        self.songs.deinit();
    }
    pub fn add(self: *Self, song: Song) !void {
        try self.songs.append(song);
    }
};
