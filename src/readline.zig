const c = @import("c.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ReadLine = struct {
    allocator: Allocator,
    const Self = @This();
    pub fn init(allocator: Allocator) Self {
        c.using_history();
        return Self {
            .allocator = allocator
        };
    }
    pub fn getLine(self: Self, prompt: ?[:0]const u8) !?[]const u8 {
        const raw = c.readline(
            if (prompt) |p| p.ptr else null
        );
        defer c.free(raw);
        if (raw == null) return null;
        const dupe = try self.allocator.dupe(u8, std.mem.span(raw));
        c.add_history(dupe.ptr);
        return dupe;
    }
    pub fn str(self: Self) []const u8 {
        return std.mem.span(self.raw);
    }
};
