const std = @import("std");
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const c = @import("c.zig");


const Self = @This();

pub const MetadataConfig = struct {
    title:  []u8,
    artist: []u8,
    album:  ?[]u8,
    year:   ?[]u8,
    path:   []const u8,
    pub fn fromMetadata(metadata: Self, path: []const u8) !MetadataConfig {
        return MetadataConfig {
            .title = metadata.title orelse return error.MissingTitle,
            .artist = metadata.artist orelse return error.MissingArtist,
            .album = metadata.album,
            .year = metadata.year,
            .path = path
        };
    }
    pub fn toJson(self: MetadataConfig, allocator: Allocator) ![]const u8 {
        return try std.json.stringifyAlloc(allocator, self, .{});
    }
    pub fn fromjson(allocator: Allocator, json: []const u8) !MetadataConfig {
        return try std.json.parseFromSlice(MetadataConfig, allocator, json, .{});
    }
};

title:  ?[]u8,
artist: ?[]u8,
album:  ?[]u8,
year:   ?[]u8,
allocator: Allocator,

fn c_str_to_optional(allocator: Allocator, c_str: [*c]u8) !?[]u8 {
    if (c_str == null or c_str[0] == 0) return null;
    return try allocator.dupe(u8, std.mem.span(c_str));
}

pub fn init(allocator: Allocator, path:  [*:0]const u8) !Self {
    const file = c.taglib_file_new(path);
    if (file == null) return error.TagLibFileNew;
    defer c.taglib_file_free(file);
    const tag = c.taglib_file_tag(file);
    return Self {
        .allocator = allocator,
        .title = try c_str_to_optional(allocator, c.taglib_tag_title(tag)),
        .artist = try c_str_to_optional(allocator, c.taglib_tag_artist(tag)),
        .album = try c_str_to_optional(allocator, c.taglib_tag_album(tag)),
        .year = try fmt.allocPrint(allocator, "{}", .{c.taglib_tag_year(tag)})
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.artist);
    self.allocator.free(self.album);
    self.allocator.free(self.title);
    self.allocator.free(self.year);
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
    \\  year: {s}
    \\}}
    , 
    .{
        self.title orelse "None",
        self.album orelse "None",
        self.artist orelse "None",
        self.year orelse "None"
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
fn ask_year(allocator: Allocator) ?[]u8 {
    while (true) {
        const response = try ask(allocator, "year: ");
        if (isValidYear(response)) {
            return response;
        } else {
            allocator.free(response);
        }
    }
}

pub fn prompt_missing(self: *Self, allocator: Allocator) !void {
    self.artist = self.artist orelse try ask(allocator, "artist: ");
    self.album = self.album orelse try ask(allocator, "album: ");
    self.title = self.title orelse try ask(allocator, "title: ");
    self.year = self.year orelse   try ask(allocator, "year: ");
}
