const std = @import("std");
const playlist = @import("playlist.zig");
const Playlist = playlist.Playlist;
const db = @import("db.zig");
const DB = @import("db.zig").DB;
const ID = @import("db.zig").ID;
const Allocator = std.mem.Allocator;
const consts = @import("constants.zig");

pub const Song = struct {
    id: ID,
    title: []const u8,
    album: ?[]const u8,
    artist: []const u8,
    path: []const u8,
    year: ?usize,
    updated: bool,
    allocator: Allocator,
    const Self = @This();
    pub fn init(allocator: Allocator, database: DB, id: ID) !Self {
        const R = &[_]type{ []const u8, ?[]const u8, []const u8, ?usize, []const u8 };
        const query = try std.fmt.allocPrint(allocator, "SELECT title,album,artist,year,path FROM songs WHERE ID={}", .{id});
        defer allocator.free(query);
        const result = try database.query(R, allocator, query);
        defer db.freeQuery(R, result);
        const data = result.items[0];
        return Self{ .id = id, .title = try allocator.dupe(u8, data[0]), .album = if (data[1]) |album| try allocator.dupe(u8, album) else null, .artist = try allocator.dupe(u8, data[2]), .year = data[3], .path = try allocator.dupe(u8, data[4]), .updated = false, .allocator = allocator };
    }
    pub fn deinit(self: Self) void {
        self.allocator.free(self.title);
        if (self.album) |album| self.allocator.free(album);
        self.allocator.free(self.artist);
        self.allocator.free(self.path);
    }
    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, out: anytype) !void {
        try out.print(
            \\Song {{
            \\  title: {s},
            \\  album: {?s},
            \\  artist: {s},
            \\ year: {?}
            \\}}
        , .{ self.title, self.album, self.artist, self.year });
    }
    pub fn save(self: Self, database: DB) !void {
        _ = database;
        // short circuit if not updated
        if (!self.updated) return;

        const str = try std.fmt.allocPrint(self.allocator, "UPDATE songs SET album={}");
        defer self.allocator.free(str);
    }
};
pub const App = struct {
    playlists: std.ArrayList(Playlist),
    db: DB,
    queue: std.ArrayList(ID),
    allocator: Allocator,
    const Self = @This();
    pub fn init(allocator: Allocator) !Self {
        return Self{ .queue = std.ArrayList(ID).init(allocator), .db = try DB.init(consts.DATABASE_PATH), .playlists = std.ArrayList(Playlist).fromOwnedSlice(allocator, try playlist.loadPlaylists(allocator, consts.PLAYLISTS_JSON_PATH)), .allocator = allocator };
    }
    pub fn deinit(self: Self) void {
        self.db.deinit();
        for (self.playlists.items) |e| {
            e.deinit();
        }
        self.playlists.deinit();
        self.queue.deinit();
    }
    pub fn getSong(self: Self, id: ID) !Song {
        return Song.init(self.allocator, self.db, id);
    }
    pub fn querySongs(self: Self, str: []const u8) []Song {
        const R = &[_]type{ i64, []const u8, ?[]const u8, []const u8, ?usize, []const u8 };
        const query = try std.fmt.allocPrint(self.allocator, "SELECT id,title,album,artist,year,path FROM songs WHERE {s}", .{str});
        defer self.allocator.free(query);
        const result = try self.db.query(R, self.allocator, query);
        defer db.freeQuery(R, result);

        const data = result.items[0];
        return Self{ .id = data[0], .title = try self.allocator.dupe(u8, data[1]), .album = if (data[2]) |album| try self.allocator.dupe(u8, album) else null, .artist = try self.allocator.dupe(u8, data[3]), .year = data[4], .path = try self.allocator.dupe(u8, data[5]), .updated = false, .allocator = self.allocator };
    }
    pub fn save(self: Self) !void {
        self.playlists.save(consts.PLAYLISTS_JSON_PATH);
    }
};
