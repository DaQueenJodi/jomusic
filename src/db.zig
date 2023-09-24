const std = @import("std");
const Metadata = @import("Metadata.zig").MetadataConfig;
const Allocator = std.mem.Allocator;
const c = @import("c.zig");
const Song = @import("app.zig").Song;


pub const ID = i64;

pub const DB = struct {
    sqlite: ?*c.sqlite3,
    const Self = @This();
    pub fn init(path: [:0]const u8) !Self {
        var sql: ?*c.sqlite3 = undefined;
        if (c.sqlite3_open(path, &sql) != c.SQLITE_OK) return error.FailedToOpenDatabase;
        return Self {
            .sqlite = sql
        };
    }
    pub fn deinit(self: Self) void {
        // assume this will always succeed; erroring in destructors is pretty stinky smelly
        _ = c.sqlite3_close(self.sqlite);
    }
    pub fn query(self: Self, comptime R: []const type, allocator: Allocator, str: []const u8) !std.ArrayList(std.meta.Tuple(R)) {
        var rows = std.ArrayList(std.meta.Tuple(R)).init(allocator);
        var stmt: ?*c.sqlite3_stmt = undefined;
        if (c.sqlite3_prepare(self.sqlite, str.ptr, @intCast(str.len), &stmt, null) != c.SQLITE_OK) return error.FailedToPrepareStatement;
        defer _ = c.sqlite3_finalize(stmt);
        while (true) {
            const res = c.sqlite3_step(stmt);
            if (res != c.SQLITE_ROW) break;
            var row: std.meta.Tuple(R) = undefined;
            std.debug.assert(R.len == c.sqlite3_column_count(stmt));
            inline for (R, 0..) |t, i| {
                const val = switch (t) {
                    []const u8 => blk: {
                        const s = c.sqlite3_column_text(stmt, i);
                        break :blk try allocator.dupe(u8, std.mem.span(s));
                    },
                    i32 => c.sqlite3_column_int(stmt, i),
                    i64 => c.sqlite3_column_int64(stmt, i),
                    usize => @as(usize, @intCast(c.sqlite3_column_int64(stmt, i))),
                    void => {},
                    // TODO: get rid of this madness by making an auxilary function that
                    // takes a type T, a stmt, an i, and an allocator and returns the value
                    else => ret: {
                        const t_info = @typeInfo(t);
                        if (t_info == .Optional) {
                            if (c.sqlite3_column_type(stmt, i) == c.SQLITE_NULL) {
                                break :ret null;
                            }
                            break :ret switch (t_info.Optional.child) {
                                i32 => c.sqlite3_column_int(stmt, i),
                                i64 => c.sqlite3_column_int64(stmt, i),
                                usize => @as(usize, @intCast(c.sqlite3_column_int64(stmt, i))),
                                []const u8 => blk: {
                                    const s = c.sqlite3_column_text(stmt, i);
                                    break :blk try allocator.dupe(u8, std.mem.span(s));
                                },
                                else => @compileError("optionals can only be i32s or strings")
                            };
                        } else {
                            @compileError("invalid type; can only be i32, i64, or a string");
                        }
                    },
                };
                row[i] = val;
            }
            try rows.append(row);
        }
        return rows;
    }
    pub fn exec(self: Self, str: []const u8) !void {
        var stmt: ?*c.sqlite3_stmt = undefined;
        if (c.sqlite3_prepare(self.sqlite, str.ptr, @intCast(str.len), &stmt, null) != c.SQLITE_OK) return error.FailedToPrepareStatement;
        defer _ = c.sqlite3_finalize(stmt);
        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) return error.FailedToExecuteStatement;
    }
    pub fn execMany(self: Self, str: []const u8) void {
        c.sqlite3_exec(self.sqlite, str, null, null, null);
    }
    fn formatValue(self: Self, allocator: Allocator, val: anytype) ![]const u8 {
        const info = @typeInfo(@TypeOf(val));
        switch (info) {
            .Optional => {
                if (val) |v| {
                    return self.formatValue(allocator, v);
                } else {
                    return allocator.dupe(u8, "NULL");
                }
            },
            .Int => return std.fmt.allocPrint(allocator, "{}", .{val}),
            @typeInfo([]u8) => return try std.fmt.allocPrint(allocator, "\"{s}\"", .{val}),
            else => @compileError("invlaid type")
        }
    }
    pub fn addSong(self: Self, allocator: Allocator, song: Metadata) !void {
        const title = try self.formatValue(allocator, song.title);
	defer allocator.free(title);
        const album = try self.formatValue(allocator, song.album);
	defer allocator.free(album);
        const artist = try self.formatValue(allocator, song.artist);
	defer allocator.free(artist);
        const year = try self.formatValue(allocator, song.year);
	defer allocator.free(year);
        const path = try self.formatValue(allocator, song.path);
	defer allocator.free(path);

        const str = try std.fmt.allocPrint(
            allocator,
            "INSERT INTO songs (title, album, artist, year, path) values ({s}, {s}, {s}, {s}, {s})",
            .{ title, album, artist, year, path }
        );
        defer allocator.free(str);
        var stmt: ?*c.sqlite3_stmt = undefined;
        if (c.sqlite3_prepare(self.sqlite, str.ptr, @intCast(str.len), &stmt, null) != c.SQLITE_OK) return error.FailedToPrepareStatement;
        defer _ = c.sqlite3_finalize(stmt);
        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) return error.FailedToExecuteStatement;
    }
    pub fn updateSong(self: Self, allocator: Allocator, song: Song) !void {
        const title = try self.formatValue(allocator, song.title);
        defer allocator.free(title);
        const album = try self.formatValue(allocator, song.album);
        defer allocator.free(album);
        const artist = try self.formatValue(allocator, song.artist);
        defer allocator.free(artist);
        const year = try self.formatValue(allocator, song.year);
        defer allocator.free(year);
        const id = try self.formatValue(allocator, song.id);
        defer allocator.free(id);
        const str = try std.fmt.allocPrint(
            allocator,
            "UPDATE songs SET title={s},album={s},artist={s},year={s} WHERE id={s}",
            .{ title, album, artist, year, id }
        );
        defer allocator.free(str);
        try self.exec(str);

    }
    pub fn pathAdded(self: Self, allocator: Allocator, path: []const u8) !bool {
        const pathf = try self.formatValue(allocator, path);
        defer allocator.free(pathf);
        const str = try std.fmt.allocPrint(
            allocator,
            "SELECT id FROM songs WHERE path={s}",
            .{pathf}
        );
        defer allocator.free(str);
        const arr = try self.query(&[_]type{ void }, allocator, str);
        return arr.items.len > 0;
    }
};


/// creates the database file and sets up the `songs` table
pub fn createDB(path: [:0]const u8) !void {
    // create empty file
    var file = try std.fs.cwd().createFile(path, .{});
    file.close();
    var db = try DB.init(path);
    defer db.deinit();
    try db.exec("CREATE TABLE songs (id INTEGER PRIMARY KEY NOT NULL, title TEXT NOT NULL, album TEXT, artist TEXT NOT NULL, year INT, path TEXT NOT NULL)");
}
pub fn freeQuery(comptime R: []const type, arr: std.ArrayList(std.meta.Tuple(R))) void {
    for (arr.items) |e| {
        inline for (R, 0..) |t, i| {
            switch (t) {
                []const u8 => arr.allocator.free(e[i]),
                ?[]const u8 => if (e[i]) |v| arr.allocator.free(v),
                else => {}
            }
        }
    }
    arr.deinit();
}
