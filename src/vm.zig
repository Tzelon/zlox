const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const compile = @import("./compiler.zig").compile;
const PrintValue = @import("./value.zig").printValue;
const debug = @import("./debug.zig");
const debug_trace_execution = debug.debug_trace_execution;

const BinaryOp = enum { ADD, SUB, MUL, DIV };
const InterpretResult = enum { INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR };
const STACK_MAX = 256;

pub const VM = struct {
    chunk: *Chunk,
    // instruction pointer - points to the instruction about to be execute
    ip: usize,
    stack: [STACK_MAX]Value = undefined,
    stack_top: usize = 0,

    pub fn init() VM {
        var vm = VM{ .chunk = undefined, .ip = undefined };
        return vm;
    }

    pub fn deinit(self: *VM) void {
        _ = self;
    }

    pub fn interpret(self: *VM, source: []const u8) InterpretResult {
        _ = self;
        compile(source);

        return InterpretResult.INTERPRET_OK;
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn run(self: *VM) InterpretResult {
        return while (true) {
            if (comptime debug_trace_execution) {
                std.debug.print("       ", .{});
                for (self.stack[0..self.stack_top]) |slot| {
                    std.debug.print("[ ", .{});
                    try PrintValue(slot);
                    std.debug.print(" ]", .{});
                }

                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(self.chunk, self.ip);
            }
            var instruction = OpCode.fromU8(self.readByte());
            switch (instruction) {
                OpCode.OP_RETURN => {
                    try PrintValue(self.pop());
                    std.debug.print("\n", .{});
                    break InterpretResult.INTERPRET_OK;
                },
                OpCode.OP_NEGATE => {
                    self.push(-self.pop());
                    continue;
                },
                OpCode.OP_ADD => {
                    self.binaryOp(.ADD);
                    continue;
                },
                OpCode.OP_SUBTRACT => {
                    self.binaryOp(.SUB);
                    continue;
                },
                OpCode.OP_MULTIPLY => {
                    self.binaryOp(.MUL);
                    continue;
                },
                OpCode.OP_DIVIDE => {
                    self.binaryOp(.DIV);
                    continue;
                },
                OpCode.OP_CONSTANT => {
                    var constant: Value = self.chunk.constants.items[self.readByte()];
                    self.push(constant);
                    std.debug.print("\n", .{});
                    continue;
                },
                else => {
                    std.debug.print("uknown instruction", .{});
                    return InterpretResult.INTERPRET_COMPILE_ERROR;
                },
            }
        };
    }

    inline fn binaryOp(self: *VM, comptime op: BinaryOp) void {
        const rhs = self.pop();
        const lhs = self.pop();

        const res = switch (op) {
            .ADD => lhs + rhs,
            .SUB => lhs - rhs,
            .MUL => lhs * rhs,
            .DIV => lhs / rhs,
        };

        self.push(res);
    }

    fn readByte(self: *VM) u8 {
        var byte = self.chunk.code.items[self.ip];
        self.ip += 1;
        return byte;
    }
};
