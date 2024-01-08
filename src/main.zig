const std = @import("std");
const io = std.io;
const process = std.process;
const Value = @import("./value.zig").Value;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const debug = @import("./debug.zig");
const VM = @import("./vm.zig").VM;
const Allocator = std.mem.Allocator;

const errout = std.io.getStdErr().writer();
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var args = try process.argsAlloc(allocator);
    defer {
        process.argsFree(allocator, args);
    }

    if (args.len == 1) {
        try repl();
    } else if (args.len == 2) {
        try runFile(allocator, args[1]);
    } else {
        std.debug.print("Usage: zlox [path]\n", .{});
        process.exit(64);
    }
}

fn repl() !void {
    var vm = VM.init();
    defer vm.deinit();
    var buf = std.io.bufferedReader(stdin);
    var reader = buf.reader();

    var line_buff: [1024]u8 = undefined;

    while (true) {
        stdout.print("> ", .{}) catch std.debug.panic("cannot write to stdout", .{});
        const line = reader.readUntilDelimiterOrEof(&line_buff, '\n') catch {
            std.debug.panic("cannot read from stdin in repl", .{});
            break;
        } orelse {
            stdout.writeAll("\n") catch std.debug.panic("cannot write to stdout", .{});
            break;
        };

        _ = vm.interpret(line);
    }
}

fn runFile(allocator: Allocator, path: []const u8) !void {
    var vm = VM.init();
    defer vm.deinit();
    const source = try std.fs.cwd().readFileAlloc(allocator, path, 1_000_000);
    defer allocator.free(source);

    _ = vm.interpret(source);
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
