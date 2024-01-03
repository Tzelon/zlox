const std = @import("std");

pub const Value = f64;

pub fn printValue(value: anytype) !void {
    std.debug.print("{d:.3}", .{value});
}
