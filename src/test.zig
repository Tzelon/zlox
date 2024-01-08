const std = @import("std");

pub fn main() !void {
    var input = "Hello, World!";
    var start = input;
    var current = input + 5; // Move current 5 bytes ahead
    const length = @as(usize, @intCast(current - start));
    std.debug.print("Length of token: {}\n", .{
        length,
    }); // Output: "Length of token: 5"
}
