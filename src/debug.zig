const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const printValue = @import("./Value.zig").printValue;

pub const debug_trace_execution = false;
pub const debug_print_code = true;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("=== {s} ===\n", .{name});
    var offset: usize = 0;

    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }

    std.debug.print("\n", .{});
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});

    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        std.debug.print("  | ", .{});
    } else {
        std.debug.print("  {d} ", .{chunk.lines.items[offset]});
    }

    const instruction = OpCode.fromU8(chunk.code.items[offset]);
    return switch (instruction) {
        OpCode.OP_NEGATE => simpleInstruction("OP_NEGATE", offset),
        OpCode.OP_CONSTANT => constantInstruction("OP_CONSTANT", chunk, offset),
        OpCode.OP_ADD => simpleInstruction("OP_ADD", offset),
        OpCode.OP_SUBTRACT => simpleInstruction("OP_SUBSTACT", offset),
        OpCode.OP_MULTIPLY => simpleInstruction("OP_MULTIPLY", offset),
        OpCode.OP_DIVIDE => simpleInstruction("OP_DIVIDE", offset),
        OpCode.OP_RETURN => simpleInstruction("OP_RETURN", offset),
        else => {
            std.debug.print("Unkown opcode {any}\n", .{instruction});
            return offset + 1;
        },
    };
}

pub fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

pub fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    var constant = chunk.code.items[offset + 1];
    std.debug.print("{s} {d: >4} '", .{ name, constant });
    try printValue(chunk.constants.items[constant]);
    std.debug.print("'\n", .{});

    return offset + 2;
}
