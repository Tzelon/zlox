const std = @import("std");
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const compile = @import("./compiler.zig").compile;
const debug = @import("./debug.zig");
const debug_trace_execution = debug.debug_trace_execution;

const BinaryOp = enum { ADD, SUB, MUL, DIV };
pub const InterpretError = error{ COMPILE_ERROR, RUNTIME_ERROR };
const InterpretResult = enum {
    INTERPRET_OK,
};
const STACK_MAX = 256;

pub const VM = struct {
    chunk: *Chunk,
    // instruction pointer - points to the instruction about to be execute
    ip: usize,
    stack: [STACK_MAX]Value = undefined,
    stack_top: usize = 0,
    allocator: Allocator,

    pub fn init(allocator: Allocator) VM {
        var vm = VM{ .allocator = allocator, .chunk = undefined, .ip = undefined };
        return vm;
    }

    pub fn deinit(self: *VM) void {
        _ = self;
    }

    pub fn interpret(self: *VM, source: []const u8) InterpretError!InterpretResult {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        if (!compile(source, &chunk)) {
            return InterpretError.COMPILE_ERROR;
        }

        self.chunk = &chunk;
        self.ip = 0;

        var result = try self.run();

        return result;
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn run(self: *VM) InterpretError!InterpretResult {
        return while (true) {
            if (comptime debug_trace_execution) {
                std.debug.print("   STACK: ", .{});
                for (self.stack[0..self.stack_top]) |slot| {
                    std.debug.print("[ ", .{});
                    try slot.printValue();
                    std.debug.print(" ]", .{});
                }

                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(self.chunk, self.ip);
            }
            var instruction = OpCode.fromU8(self.readByte());
            switch (instruction) {
                OpCode.OP_RETURN => {
                    // try PrintValue(self.pop());
                    try self.pop().printValue();
                    std.debug.print("\n", .{});
                    break InterpretResult.INTERPRET_OK;
                },
                OpCode.OP_NEGATE => {
                    if (!self.peek(0).isA(.number)) {
                        self.runtimeError("Operand must be a number.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    self.push(Value.fromNumber(-self.pop().number));
                    continue;
                },
                OpCode.OP_ADD => {
                    try self.binaryOp(.ADD);
                    continue;
                },
                OpCode.OP_SUBTRACT => {
                    try self.binaryOp(.SUB);
                    continue;
                },
                OpCode.OP_MULTIPLY => {
                    try self.binaryOp(.MUL);
                    continue;
                },
                OpCode.OP_DIVIDE => {
                    try self.binaryOp(.DIV);
                    continue;
                },
                OpCode.OP_CONSTANT => {
                    var constant: Value = self.chunk.constants.items[self.readByte()];
                    self.push(constant);
                    continue;
                },
                else => {
                    std.debug.print("uknown instruction", .{});
                    return InterpretError.COMPILE_ERROR;
                },
            }
        };
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack[self.stack_top - 1 - distance];
    }

    fn runtimeError(self: *VM, comptime message: []const u8, args: anytype) void {
        const err_writer = std.io.getStdErr().writer();

        err_writer.print(message ++ ".\n", args) catch {};

        const instruction = self.ip - 1;
        const line = self.chunk.lines.items[instruction];
        err_writer.print("[line {d}] in script\n", .{line}) catch {};

        self.resetStack();
    }

    fn resetStack(self: *VM) void {
        self.stack_top = 0;
    }

    inline fn binaryOp(self: *VM, comptime op: BinaryOp) InterpretError!void {
        if (!self.peek(0).isA(.number) or !self.peek(1).isA(.number)) {
            self.runtimeError("Operands must be numbers", .{});
            return InterpretError.RUNTIME_ERROR;
        }

        const rhs = self.pop().number;
        const lhs = self.pop().number;

        const res = switch (op) {
            .ADD => lhs + rhs,
            .SUB => lhs - rhs,
            .MUL => lhs * rhs,
            .DIV => lhs / rhs,
        };

        self.push(Value.fromNumber(res));
    }

    fn readByte(self: *VM) u8 {
        var byte = self.chunk.code.items[self.ip];
        self.ip += 1;
        return byte;
    }
};
