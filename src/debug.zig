const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const printValue = @import("./Value.zig").printValue;

pub const debug_trace_execution = false;
pub const debug_print_code = false;
pub const debug_stress_gc = false;
pub const debug_log_gc = true;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("=== {s} ===\n", .{name});
    std.debug.print("offset  line  instruction        index value\n", .{});
    std.debug.print("---------------------------------------------\n", .{});
    var offset: usize = 0;

    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }

    std.debug.print("\n", .{});
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("+++ {d:0>4} ", .{offset});

    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        std.debug.print("    |    ", .{});
    } else {
        std.debug.print("    {d}    ", .{chunk.lines.items[offset]});
    }

    const instruction = OpCode.fromU8(chunk.code.items[offset]);
    return switch (instruction) {
        OpCode.OP_NEGATE => simpleInstruction("OP_NEGATE", offset),
        OpCode.OP_CONSTANT => constantInstruction("OP_CONSTANT", chunk, offset),
        OpCode.OP_CLOSE_UPVALUE => simpleInstruction("OP_CLOSE_UPVALUE", chunk, offset),
        OpCode.OP_ADD => simpleInstruction("OP_ADD", offset),
        OpCode.OP_SUBTRACT => simpleInstruction("OP_SUBSTACT", offset),
        OpCode.OP_MULTIPLY => simpleInstruction("OP_MULTIPLY", offset),
        OpCode.OP_DIVIDE => simpleInstruction("OP_DIVIDE", offset),
        OpCode.OP_JUMP => jumpInstruction("OP_JUMP", 1, chunk, offset),
        OpCode.OP_JUMP_IF_FALSE => jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        OpCode.OP_RETURN => simpleInstruction("OP_RETURN", offset),
        OpCode.OP_NIL => simpleInstruction("OP_NIL", offset),
        OpCode.OP_TRUE => simpleInstruction("OP_TRUE", offset),
        OpCode.OP_FALSE => simpleInstruction("OP_FALSE", offset),
        OpCode.OP_NOT => simpleInstruction("OP_NOT", offset),
        OpCode.OP_POP => simpleInstruction("OP_POP", offset),
        OpCode.OP_CALL => byteInstruction("OP_CALL", chunk, offset),
        OpCode.OP_CLOSURE => closureInstruction("OP_CLOSURE", chunk, offset),
        OpCode.OP_GET_LOCAL => byteInstruction("OP_GET_LOCAL", chunk, offset),
        OpCode.OP_SET_LOCAL => byteInstruction("OP_SET_LOCAL", chunk, offset),
        OpCode.OP_GET_UPVALUE => byteInstruction("OP_GET_UPVALUE", chunk, offset),
        OpCode.OP_SET_UPVALUE => byteInstruction("OP_SET_UPVALUE", chunk, offset),
        OpCode.OP_GET_GLOBAL => constantInstruction("OP_GET_GLOBAL", chunk, offset),
        OpCode.OP_SET_GLOBAL => constantInstruction("OP_SET_GLOBAL", chunk, offset),
        OpCode.OP_DEFINE_GLOBAL => constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        OpCode.OP_EQUAL => simpleInstruction("OP_EQUAL", offset),
        OpCode.OP_GREATER => simpleInstruction("OP_GREATER", offset),
        OpCode.OP_LESS => simpleInstruction("OP_LESS", offset),
        OpCode.OP_PRINT => simpleInstruction("OP_PRINT", offset),
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

pub fn jumpInstruction(name: []const u8, sign: u16, chunk: *Chunk, offset: usize) usize {
    var jump = @as(u16, chunk.code.items[offset + 1]) << 8;
    jump |= chunk.code.items[offset + 2];
    std.debug.print("{s: <20}     {d} -> {d}\n", .{ name, offset, offset + 3 + sign * jump });

    return offset + 3;
}

pub fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    std.debug.print("{s: <20} {d}   '", .{ name, constant });
    try chunk.constants.items[constant].printValue();
    std.debug.print("'\n", .{});

    return offset + 2;
}

pub fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const slot = chunk.code.items[offset + 1];
    std.debug.print("{s: <20} {d}\n", .{ name, slot });

    return offset + 2;
}

pub fn closureInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    offset += 1;
    const constant = chunk.code.items[offset];
    offset += 1;
    std.debug.print("{s: <20} {d}\n", .{ name, constant });
    chunk.constants.items[constant].printValue();
    std.debug.print("\n", .{});

    const func = chunk.constants.items[constant].obj.asFunction();
    var i: usize = 0;
    while (i < func.upvalue_count) : (i += 1) {
        const is_local = chunk.code.items[offset];
        const value_type = if (is_local) "local" else "upvalue";
        offset += 1;
        const index = chunk.code.items[offset];
        offset += 1;
        std.debug.print("{} | {s} {}\n", .{ offset - 2, value_type, index });
    }

    return offset;
}
