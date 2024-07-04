const std = @import("std");
const Value = @import("./value.zig").Value;
const Allocator = std.mem.Allocator;

// instructions.
pub const OpCode = enum(u8) {
    OP_RETURN,
    OP_CONSTANT,
    OP_CLOSE_UPVALUE,
    OP_CLOSURE,
    OP_CLASS,
    OP_INHERIT,
    OP_GET_SUPER,
    OP_SUPER_INVOKE,
    OP_METHOD,
    OP_INVOKE,
    OP_NEGATE,
    OP_PRINT,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_JUMP,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_NOT,
    OP_POP,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_DEFINE_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    ELSE,

    pub fn toU8(self: OpCode) u8 {
        return @intFromEnum(self);
    }

    pub fn fromU8(byte: u8) OpCode {
        return @enumFromInt(byte);
    }
};

// sequences of bytcodes that represent our code.
pub const Chunk = struct {
    // array of instructions
    code: std.ArrayList(u8),
    // array of values to be stored
    constants: std.ArrayList(Value),
    // array of line number, each number in the array is the line number of the corresponding byte in the bytecode.
    lines: std.ArrayList(usize),

    pub fn init(allocator: Allocator) Chunk {
        return Chunk{ .code = std.ArrayList(u8).init(allocator), .lines = std.ArrayList(usize).init(allocator), .constants = std.ArrayList(Value).init(allocator) };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    /// write code into chunk with line number
    pub fn writeCode(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    /// write OpCode into chunk with line number.
    /// convert the OpCode to u8 before inset it to the chunk
    pub fn writeOpCode(self: *Chunk, opcode: OpCode, line: usize) !void {
        try self.writeCode(opcode.toU8(), line);
    }

    pub fn addConstant(self: *Chunk, constant: Value) !u8 {
        try self.constants.append(constant);
        return @truncate(self.constants.items.len - 1);
    }
};
