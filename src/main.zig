const std = @import("std");
const Value = @import("./value.zig").Value;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const debug = @import("./debug.zig");
const VM = @import("./vm.zig").VM;
const Allocator = std.mem.Allocator;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var vm = VM.init();
    var chunk = Chunk.init(allocator);
    defer {
        vm.deinit();
        chunk.deinit();
    }

    var constant = try chunk.addConstant(1.2);
    try chunk.writeOpCode(OpCode.OP_CONSTANT, 124);
    try chunk.writeCode(constant, 124);

    constant = try chunk.addConstant(3.4);
    try chunk.writeOpCode(OpCode.OP_CONSTANT, 124);
    try chunk.writeCode(constant, 124);

    try chunk.writeOpCode(OpCode.OP_ADD, 124);

    constant = try chunk.addConstant(5.6);
    try chunk.writeOpCode(OpCode.OP_CONSTANT, 124);
    try chunk.writeCode(constant, 124);

    try chunk.writeOpCode(OpCode.OP_DIVIDE, 124);
    try chunk.writeOpCode(OpCode.OP_NEGATE, 124);

    try chunk.writeOpCode(OpCode.OP_RETURN, 124);

    debug.disassembleChunk(&chunk, "test chunk");
    _ = vm.interpret(&chunk);
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
