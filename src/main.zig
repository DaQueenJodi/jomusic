const std = @import("std");
const c = @import("c.zig");
const db = @import("db.zig");
const DB = db.DB;
const Metadata = @import("Metadata.zig");
const audio = @import("audio.zig");
const playlist = @import("playlist.zig");
const App = @import("app.zig").App;
const consts = @import("constants.zig");
const rl = @import("readline.zig");

const Allocator = std.mem.Allocator;

const SONG = "music/welp.mp3";

pub fn indexDir(database: DB, allocator: Allocator, dir: []const u8) !void {
    var iterableDir = try std.fs.cwd().openIterableDir(dir, .{});
    defer iterableDir.close();
    var iter = iterableDir.iterate();
    while (try iter.next()) |entry| {
        const path = try std.fs.path.joinZ(allocator, &[_][]const u8{ dir, entry.name });
        defer allocator.free(path);
        if (entry.kind == .directory) {
            try indexDir(database, allocator, path);
            continue;
        }
        if (entry.kind != .file) {
            std.log.info("path `{s}` points to a file of kind {}, skipping...", .{ path, entry.kind });
            continue;
        }
        if (try database.pathAdded(allocator, path)) {
            std.log.info("path `{s}` already added, skipping...", .{path});
            continue;
        }
        var metadata = try Metadata.init(allocator, path);
        defer metadata.deinit();
        if (metadata.artist == null or
            metadata.title == null or
            metadata.year == null or
            metadata.album == null)
        {
            try metadata.prompt_missing();
        }
        try db.addSong(allocator, try Metadata.MetadataConfig.fromMetadata(metadata));
    }
}

const Command = enum {
    help,
    exit,
    list,
    playlists,
    save,
    pub fn help(self: Command) !void {
        const stdout = std.io.getStdOut().writer();
        switch (self) {
            .help => try stdout.writeAll(
                \\Usage: help [cmd]
                \\displays information on how to use the program, or if provided, a command
                \\
            ),
            .exit => try stdout.writeAll(
                \\Usage: exit
                \\exits the program
                \\
            ),
            .list => try stdout.writeAll(
                \\Usage: ls [playlist]
                \\lists the songs in a playlist
                \\
            ),
            .playlists => try stdout.writeAll(
                \\Usage: playlists {list,create,remove}
                \\manage playlists:
                \\  list: list available playlists
                \\  create [name]: creates a new playlist named `name`
                \\  remove [name]: removes a playlist called `name`
                \\
            ),
            .save => try stdout.writeAll(
                \\Usage: save
                \\save changes to file
                \\
            ),
        }
    }
    fn comptimeConcat(comptime strs: [][]const u8) []const u8 {
        comptime {
            var n = 0;
            for (strs) |str| {
                n += str.len;
            }
            var buff: [n]u8 = undefined;
            var offset = 0;
            for (strs) |str| {
                for (str, 0..) |char, i| {
                    buff[i + offset] = char;
                }
                offset += str.len;
            }
            return &buff;
        }
    }
    pub fn run(self: Command, allocator: Allocator, rest: []const u8, app: *App) !void {
        const writer = std.io.getStdOut().writer();
        switch (self) {
            .help => {
                if (rest.len == 0) {
                    try writer.writeAll("Available commands:\n" ++
                        comptime comptimeConcat(blk: {
                        const fields = @typeInfo(Command).Enum.fields;
                        var names: [fields.len][]const u8 = undefined;
                        inline for (fields, 0..) |field, i| {
                            names[i] = "    " ++ field.name ++ "\n";
                        }
                        break :blk &names;
                    }));
                } else {
                    const cmd = cmpEnum(Command, rest) orelse {
                        try writer.print("invalid command: `{s}`\n", .{rest});
                        return error.BadUsage;
                    };
                    try cmd.help();
                }
            },
            .list => {
                if (rest.len == 0) {
                    try writer.writeAll("you must supply a playlist name\n");
                    return error.BadUsage;
                }
                const playlist_name = rest;
                const ids = blk: {
                    for (app.playlists.items) |pl| {
                        if (std.mem.eql(u8, pl.name, playlist_name)) {
                            break :blk pl.songs.items;
                        }
                        try writer.print("playlist not found: `{s}`\n", .{rest});
                        return error.InvalidPlaylistName;
                    } else {
                        try writer.writeAll("no playlists available, create one with that name\n");
                        return error.NoPlaylists;
                    }
                };
                for (ids) |id| {
                    const song = try app.getSong(id);
                    defer song.deinit();
                    try writer.print("{any}\n", .{song});
                }
            },
            .playlists => {
                var iter = std.mem.splitScalar(u8, rest, ' ');
                const subcommand = iter.next() orelse {
                    try writer.writeAll("you must supply a subcommand\n");
                    return error.BadUsage;
                };
                if (std.ascii.eqlIgnoreCase(subcommand, "create")) {
                    const name = iter.next() orelse {
                        try writer.writeAll("you must supply a name\n");
                        return error.BadUsage;
                    };
                    for (app.playlists.items) |pl| {
                        if (std.mem.eql(u8, name, pl.name)) {
                            try writer.print("playlist `{s}` already exists!\n", .{name});
                            return error.PlaylistAlreadyExists;
                        }
                    }
                    try app.playlists.append(try playlist.Playlist.init(allocator, name));
                } else if (std.ascii.eqlIgnoreCase(subcommand, "remove")) {
                    const name = iter.next() orelse {
                        try writer.writeAll("you must supply a name\n");
                        return error.BadUsage;
                    };
                    for (app.playlists.items, 0..) |pl, i| {
                        if (std.mem.eql(u8, name, pl.name)) {
                            _ = app.playlists.swapRemove(i);
                            return;
                        }
                    }
                    try writer.print("playlist `{s}` not found!\n", .{name});
                    return error.PlaylistNotFound;
                } else if (std.ascii.eqlIgnoreCase(subcommand, "list")) {
                    for (app.playlists.items, 0..) |pl, i| {
                        try writer.print("{}: {s}\n", .{ i + 1, pl.name });
                    }
                } else {
                    try writer.print("invalid subcommand: `{s}`\n", .{subcommand});
                    return error.BadUsage;
                }
            },
            .save => {
                try app.save();
            },
            .exit => unreachable,
        }
    }
};

fn cmpEnum(comptime T: type, str: []const u8) ?T {
    const info = @typeInfo(T);
    if (info != .Enum) @compileError("T must be an enum");
    inline for (info.Enum.fields) |field| {
        const name = field.name;
        if (std.mem.eql(u8, str, name)) return @enumFromInt(field.value);
    }
    return null;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        if (gpa.deinit() != .ok) std.debug.print("whoops, we leaked :(\n", .{});
    }
    var app = try App.init(allocator);
    defer app.deinit();

    const readline = rl.ReadLine.init(allocator);
    while (true) {
        const line = try readline.getLine("> ") orelse continue;
        defer allocator.free(line);
        var split = std.mem.splitScalar(u8, line, ' ');
        const cmd_str = split.next() orelse return error.NoCommand;
        const cmd = cmpEnum(Command, cmd_str) orelse return error.InvalidCommand;
        if (cmd == .exit) {
            break;
        }
        var rest = line;
        rest.ptr += cmd_str.len + 1;
        rest.len -= cmd_str.len + 1;
        cmd.run(allocator, rest, &app) catch |err| {
            switch (err) {
                error.BadUsage => try cmd.help(),
                else => {},
            }
        };
    }
}
