const std = @import("std");
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const Obj = @import("./object.zig").Obj;
const Table = @import("./table.zig").Table;
const compile = @import("./compiler.zig").compile;
const debug = @import("./debug.zig");
const debug_trace_execution = debug.debug_trace_execution;

const BinaryOp = enum { ADD, SUB, MUL, DIV, LESS, GREATER };
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
    strings: Table,
    /// pointer to the head of the objects list
    objects: ?*Obj,
    globals: Table,

    pub fn init(allocator: Allocator) VM {
        return VM{ .globals = Table.init(allocator), .strings = Table.init(allocator), .objects = null, .allocator = allocator, .chunk = undefined, .ip = undefined };
    }

    pub fn deinit(self: *VM) void {
        self.strings.deinit();
        self.globals.deinit();
        self.freeObjects();
    }

    pub fn interpret(self: *VM, source: []const u8) InterpretError!InterpretResult {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        if (!compile(self, source, &chunk)) {
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
                    break InterpretResult.INTERPRET_OK;
                },
                OpCode.OP_LOOP => {
                    const offset = self.readShort();
                    self.ip -= offset;
                    continue;
                },
                OpCode.OP_JUMP_IF_FALSE => {
                    const offset = self.readShort();
                    if (isFalsey(self.peek(0))) {
                        self.ip += offset;
                        continue;
                    }
                },
                OpCode.OP_JUMP => {
                    const offset = self.readShort();
                    self.ip += offset;
                    continue;
                },
                OpCode.OP_NEGATE => {
                    if (!self.peek(0).isA(.number)) {
                        self.runtimeError("Operand must be a number.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    self.push(Value.fromNumber(-self.pop().number));
                    continue;
                },
                OpCode.OP_GREATER => {
                    try self.binaryOp(.GREATER);
                    continue;
                },
                OpCode.OP_LESS => {
                    try self.binaryOp(.LESS);
                    continue;
                },
                OpCode.OP_ADD => {
                    if (Obj.isA(self.peek(0), .String) and Obj.isA(self.peek(1), .String)) {
                        self.concatenate();
                    } else {
                        try self.binaryOp(.ADD);
                    }
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
                    var constant = self.readConstant();
                    self.push(constant);
                    continue;
                },
                OpCode.OP_NIL => {
                    self.push(Value.fromNil());
                    continue;
                },
                OpCode.OP_TRUE => {
                    self.push(Value.fromBool(true));
                    continue;
                },
                OpCode.OP_FALSE => {
                    self.push(Value.fromBool(false));
                    continue;
                },
                OpCode.OP_POP => {
                    _ = self.pop();
                    continue;
                },
                OpCode.OP_GET_LOCAL => {
                    const slot = self.readByte();
                    self.push(self.stack[slot]);
                    continue;
                },
                OpCode.OP_SET_LOCAL => {
                    const slot = self.readByte();
                    self.stack[slot] = self.peek(0);
                    continue;
                },
                OpCode.OP_GET_GLOBAL => {
                    const name = self.readString();
                    var val = Value.fromNil();
                    if (!self.globals.get(name, &val)) {
                        self.runtimeError("Undefined variable {s}", .{name.chars});
                    }

                    self.push(val);

                    continue;
                },
                OpCode.OP_SET_GLOBAL => {
                    const name = self.readString();
                    // we are not allow to assign for a new variable
                    if (self.globals.set(name, self.peek(0))) {
                        _ = self.globals.delete(name);
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});

                        return InterpretError.RUNTIME_ERROR;
                    }
                    continue;
                },
                OpCode.OP_DEFINE_GLOBAL => {
                    const name = self.readString();
                    _ = self.globals.set(name, self.peek(0));

                    //Note that we don’t pop the value until after we add it to the hash table.
                    //That ensures the VM can still find the value if a garbage collection is triggered right in the middle of adding it to the hash table.
                    //That’s a distinct possibility since the hash table requires dynamic allocation when it resizes.
                    _ = self.pop();
                    continue;
                },
                OpCode.OP_EQUAL => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value.fromBool(a.equal(b)));
                    continue;
                },
                OpCode.OP_NOT => {
                    self.push(Value.fromBool(isFalsey(self.pop())));
                    continue;
                },
                OpCode.OP_PRINT => {
                    try self.pop().printValue();
                    std.debug.print("\n", .{});
                    continue;
                },
                else => {
                    std.debug.print("uknown instruction {} \n", .{instruction});
                    return InterpretError.COMPILE_ERROR;
                },
            }
        };
    }

    fn concatenate(self: *VM) void {
        const b = self.pop().obj.asString();
        const a = self.pop().obj.asString();
        const heap = std.mem.concat(self.allocator, u8, &[_][]const u8{ a.chars, b.chars }) catch unreachable;
        const obj = Obj.String.take(self, heap);

        self.push(Value.fromObj(&obj.obj));
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack[self.stack_top - 1 - distance];
    }

    fn isFalsey(value: Value) bool {
        return switch (value) {
            .nil => true,
            .boolean => |val| !val,
            else => false,
        };
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

        switch (op) {
            .ADD => self.push(Value.fromNumber(lhs + rhs)),
            .SUB => self.push(Value.fromNumber(lhs - rhs)),
            .MUL => self.push(Value.fromNumber(lhs * rhs)),
            .DIV => self.push(Value.fromNumber(lhs / rhs)),
            .GREATER => self.push(Value.fromBool(lhs > rhs)),
            .LESS => self.push(Value.fromBool(lhs < rhs)),
        }
    }

    fn readByte(self: *VM) u8 {
        var byte = self.chunk.code.items[self.ip];
        self.ip += 1;
        return byte;
    }

    inline fn readConstant(self: *VM) Value {
        return self.chunk.constants.items[self.readByte()];
    }

    /// reads two 16bits operand
    inline fn readShort(self: *VM) u16 {
        self.ip += 2;
        const items = self.chunk.code.items;
        return (@as(u16, items[self.ip - 2]) << 8) | items[self.ip - 1];
    }

    inline fn readString(self: *VM) *Obj.String {
        return self.readConstant().obj.asString();
    }

    fn freeObjects(self: *VM) void {
        var object = self.objects;

        while (object) |o| {
            const next = o.next;
            o.destroy(self);
            object = next;
        }
    }
};
