const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;

pub fn compile(source: []const u8) void {
    var scanner = Scanner.init(source);
    var line: i32 = -1;
    while (true) {
        var token = scanner.scanToken();
        if (token.line != line) {
            std.debug.print("{d: <3}", .{token.line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print("{any} '{s}'  \n", .{ token.type, token.lexeme });

        if (token.type == .TOKEN_EOF) break;
    }
}
