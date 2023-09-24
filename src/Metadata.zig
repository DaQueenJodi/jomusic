const std = @import("std");
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const c = @import("c.zig");


const Self = @This();

pub const MetadataConfig = struct {
    title:  []u8,
    artist: []u8,
    album:  ?[]u8,
    year:   ?usize,
    path:   []const u8,
    pub fn fromMetadata(metadata: Self) !MetadataConfig {
        return MetadataConfig {
            .title = metadata.title orelse return error.MissingTitle,
            .artist = metadata.artist orelse return error.MissingArtist,
            .album = metadata.album,
            .year = metadata.year,
            .path = metadata.path
        };
    }
};

title:  ?[]u8,
artist: ?[]u8,
album:  ?[]u8,
year:   ?usize,
path:   []const u8,
allocator: Allocator,

fn c_str_to_optional(allocator: Allocator, c_str: [*c]u8) !?[]u8 {
    if (c_str == null or c_str[0] == 0) return null;
    return try allocator.dupe(u8, std.mem.span(c_str));
}

pub fn init(allocator: Allocator, path:  [:0]const u8) !Self {
    const file = c.taglib_file_new(path);
    if (file == null) return error.TagLibFileNew;
    defer c.taglib_file_free(file);
    const tag = c.taglib_file_tag(file);
    return Self {
        .allocator = allocator,
        .title = try c_str_to_optional(allocator, c.taglib_tag_title(tag)),
        .artist = try c_str_to_optional(allocator, c.taglib_tag_artist(tag)),
        .album = try c_str_to_optional(allocator, c.taglib_tag_album(tag)),
        .year = c.taglib_tag_year(tag),
        .path = path
    };
}

pub fn deinit(self: *Self) void {
    if (self.title)  |title|  self.allocator.free(title);
    if (self.artist) |artist| self.allocator.free(artist);
    if (self.album)  |album|  self.allocator.free(album);
}

pub fn format(
    self: Self,
    comptime _: []const u8,
    _: fmt.FormatOptions,
    out: anytype
) !void {
    try out.print(
    \\Metadata {{ 
    \\  title: {s},
    \\  album: {s},
    \\  artist: {s},
    \\  year: {}
    \\}}
    , 
    .{
        self.title orelse "None",
        self.album orelse "None",
        self.artist orelse "None",
        self.year
    }
    );
}

fn ask(allocator: Allocator, prompt: []const u8) !?[]u8 {
    const out = std.io.getStdOut().writer();
    const in = std.io.getStdIn().reader();
    try out.writeAll(prompt);
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();
    const writer = buf.writer();
    
    try in.streamUntilDelimiter(writer, '\n', null);
    if (buf.items.len == 0) return null;
    return try buf.toOwnedSlice();
}

fn isValidYear(str: []u8) bool {
    for (str) |char| {
        if (!std.ascii.isDigit(char)) return false;
    }
    return true;
}
fn ask_year(allocator: Allocator) !?usize {
    while (true) {
        const response = try ask(allocator, "year: ");
        if (response) |resp| {
            if (isValidYear(resp)) {
                return try std.fmt.parseInt(usize, resp, 10);
            } else {
                allocator.free(resp);
            }
        } else {
            return null;
        }
    }
}

pub fn prompt_missing(self: *Self) !void {
    const stdout = std.io.getStdOut().writer();
    const allocator = self.allocator;
    try stdout.print("please fill in the missing information for this file: {s}\n", .{self.path});
    // TODO: make this less repetative, it's small so it's okay but like
    if (self.title) |title| {
       try  stdout.print("title {s}\n", .{title});
    }
    if (self.artist) |artist| {
        try stdout.print("artist {s}\n", .{artist});
    }
    if (self.album) |album| {
        try stdout.print("album {s}\n", .{album});
    }
    if (self.year) |year| {
        try stdout.print("year {}\n", .{year});
    }
    self.artist = self.artist orelse try ask(allocator, "artist: ");
    self.album  = self.album  orelse try ask(allocator, "album: ");
    self.title  = self.title  orelse try ask(allocator, "title: ");
    self.year   = self.year   orelse try ask_year(allocator);
}
