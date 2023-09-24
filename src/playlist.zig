const std = @import("std");
const db = @import("db.zig");


const Allocator = std.mem.Allocator;

pub const Playlist = struct {
    songs: std.ArrayList(db.ID),
    name: []const u8,
    const Self = @This();
    pub fn deinit(self: Self, allocator: Allocator) void {
        self.songs.deinit();
        allocator.free(self.name);
    }
    pub fn shuffle(self: Self, allocator: Allocator) []const db.ID {
        // make a copy so we don't overwrite the original list
        var result = allocator.dupe(db.ID, self.songs);
        // TODO: give this a proper seed
        const random = std.rand.DefaultPrng.init(0).random();
        random.shuffle(db.ID, result);
        return result;
    }
};

pub const PlaylistJson = struct {
    songs: []const db.ID,
    name: []const u8
};

pub fn loadPlaylists(allocator: Allocator, path: []const u8) ![]Playlist {
    // TODO: maybe file reading should be a function outside of this?
    // I don't think I need it anywhere else though
    if (std.fs.cwd().openFile(path, .{})) |file| {
        defer file.close();
        const stat = try file.stat();
        const len = @as(usize, stat.size);
        const buf = try allocator.alloc(u8, len);
        defer allocator.free(buf);
        std.debug.assert(try file.readAll(buf) == len);
        const parsed = try std.json.parseFromSlice([]PlaylistJson, allocator, buf, .{});
        defer parsed.deinit();
        const value = parsed.value;
        // copy everything so that it doesn't get invalidated on deinit
        const copy = try allocator.alloc(Playlist, value.len);
        for (value, 0..) |v, i| {
            copy[i] = .{
                .name = try allocator.dupe(u8, v.name),
                .songs = blk: {
                    var songs = std.ArrayList(db.ID).init(allocator);
                    try songs.appendSlice(v.songs);
                    break :blk songs;
                }
            };
        }
        return copy;
    } else |err| {
        return switch (err) {
            error.FileNotFound => return &[0]Playlist {},
            else => err
        };
    }
}
