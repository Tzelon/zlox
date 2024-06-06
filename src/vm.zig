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
const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * std.math.maxInt(u8);

const CallFrame = struct {
    closure: *Obj.Closure,
    // instruction pointer - points to the instruction about to be execute
    ip: usize,
    // points into the VM’s value stack at the first slot that this function can use
    slot: usize,
};

pub fn clockNative(_: []const Value) Value {
    return Value{ .number = @as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000 };
}

pub const VM = struct {
    frames: [FRAMES_MAX]CallFrame = undefined,
    frame_count: u32 = 0,
    stack: [STACK_MAX]Value = undefined,
    stack_top: usize = 0,
    allocator: Allocator,
    strings: Table,
    /// pointer to the head of the objects list
    objects: ?*Obj,
    globals: Table,

    pub fn init(allocator: Allocator) VM {
        var vm = VM{ .globals = Table.init(allocator), .strings = Table.init(allocator), .objects = null, .allocator = allocator };

        vm.defineNative("clock", clockNative);

        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.strings.deinit();
        self.globals.deinit();
        self.freeObjects();
    }

    pub fn interpret(self: *VM, source: []const u8) InterpretError!InterpretResult {
        const function = compile(
            self,
            source,
        ) catch return InterpretError.COMPILE_ERROR;

        self.push(Value.fromObj(&function.obj));
        const closure = Obj.Closure.create(self, function);
        _ = self.pop();
        self.push(Value.fromObj(&closure.obj));
        _ = self.call(closure, 0);

        const result = try self.run();

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
        var frame = self.currentFrame();

        return while (true) {
            if (comptime debug_trace_execution) {
                std.debug.print("   STACK: ", .{});
                for (self.stack[0..self.stack_top]) |slot| {
                    std.debug.print("[ ", .{});
                    try slot.printValue();
                    std.debug.print(" ]", .{});
                }

                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(frame.closure.function.chunk, frame.ip - frame.closure.function.chunk.code);
            }
            const instruction = OpCode.fromU8(self.readByte());
            switch (instruction) {
                OpCode.OP_CALL => {
                    const arg_acount = self.readByte();
                    if (!self.callValue(self.peek(arg_acount), arg_acount)) {
                        return InterpretError.RUNTIME_ERROR;
                    }
                    frame = &self.frames[self.frame_count - 1];
                    continue;
                },
                OpCode.OP_CLOSURE => {
                    const func = self.readConstant().obj.asFunction();
                    const closure = Obj.Closure.create(self, func);
                    self.push(Value.fromObj(&closure.obj));

                    var i: usize = 0;
                    while (i < closure.upvalue_count) : (i += 1) {
                        const is_local = self.readByte();
                        const index = self.readByte();

                        if (is_local == 1) {
                            closure.upvalues[i] = self.captureUpvalue(&self.stack[self.currentFrame().slot + index]);
                        } else {
                            closure.upvalues[i] = self.currentFrame().closure.upvalues[index];
                        }
                    }

                    continue;
                },
                OpCode.OP_RETURN => {
                    const result = self.pop();
                    self.frame_count -= 1;

                    if (self.frame_count == 0) {
                        _ = self.pop();
                        break InterpretResult.INTERPRET_OK;
                    }

                    self.stack_top = frame.slot;
                    self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                    continue;
                },
                OpCode.OP_LOOP => {
                    const offset = self.readShort();
                    frame.ip -= offset;
                    continue;
                },
                OpCode.OP_JUMP_IF_FALSE => {
                    const offset = self.readShort();
                    if (isFalsey(self.peek(0))) {
                        frame.ip += offset;
                        continue;
                    }
                },
                OpCode.OP_JUMP => {
                    const offset = self.readShort();
                    frame.ip += offset;
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
                    const constant = self.readConstant();
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
                OpCode.OP_GET_UPVALUE => {
                    const slot = self.readByte();
                    self.push(self.currentFrame().closure.upvalues[slot].location.*);
                    continue;
                },
                OpCode.OP_SET_UPVALUE => {
                    const slot = self.readByte();
                    self.currentFrame().closure.upvalues[slot].location.* = self.peek(0);
                    continue;
                },
                OpCode.OP_GET_LOCAL => {
                    const slot = self.readByte();
                    // put the index in a new veriable and not inline it, increase the performence by 30x not sure why.
                    const new_slot = self.currentFrame().slot + slot;
                    // we need to offset the stack pointer by the current frame pointer
                    self.push(self.stack[new_slot]);
                    continue;
                },
                OpCode.OP_SET_LOCAL => {
                    const slot = self.readByte();
                    // put the index in a new veriable and not inline it, increase the performence by 30x not sure why.
                    const new_slot = self.currentFrame().slot + slot;
                    // we need to offset the stack pointer by the current frame pointer
                    self.stack[new_slot] = self.peek(0);
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

        var i = self.frame_count;
        while (i > 0) {
            i -= 1;

            const frame = &self.frames[i];
            const function = frame.closure.function;
            const instruction = frame.ip - 1;

            err_writer.print("[line {d}] ", .{function.chunk.lines.items[instruction]}) catch {};

            if (function.name == null) {
                err_writer.print("script\n", .{}) catch {};
            } else {
                err_writer.print("{s}()\n", .{function.name.?.chars}) catch {};
            }
        }

        self.resetStack();
    }

    fn defineNative(self: *VM, name: []const u8, function: Obj.Native.NativeFn) void {
        self.push(Value.fromObj(&Obj.String.copy(self, name).obj));
        self.push(Value.fromObj(&Obj.Native.create(self, function).obj));

        _ = self.globals.set(Obj.asString(self.stack[0].obj), self.stack[1]);

        _ = self.pop();
        _ = self.pop();
    }

    fn resetStack(self: *VM) void {
        self.stack_top = 0;
        self.frame_count = 0;
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

    fn callValue(self: *VM, callee: Value, arg_acount: u8) bool {
        switch (callee) {
            .obj => |obj| {
                switch (obj.obj_type) {
                    Obj.Type.Native => {
                        const native = obj.asNative();
                        const args = self.stack[self.stack_top - arg_acount - 1 ..];
                        const result = native.function(args);
                        self.stack_top -= arg_acount + 1;
                        self.push(result);
                        return true;
                    },
                    Obj.Type.Closure => {
                        return self.call(obj.asClosure(), arg_acount);
                    },
                    else => {
                        self.runtimeError("Can only call functions and classes.", .{});
                        return false;
                    },
                }
            },
            else => {
                self.runtimeError("Can only call functions and classes", .{});
                return false;
            },
        }
    }

    fn captureUpvalue(self: *VM, local: *Value) *Obj.Upvalue {
        const createdUpvalue = Obj.Upvalue.create(self, local);

        return createdUpvalue;
    }

    fn call(self: *VM, closure: *Obj.Closure, arg_acount: u8) bool {
        if (arg_acount != closure.function.arity) {
            self.runtimeError("Expected {d} arguments but got {d}", .{ closure.function.arity, arg_acount });
            return false;
        }

        if (self.frame_count == FRAMES_MAX) {
            self.runtimeError("Stack overflow.", .{});
            return false;
        }

        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.closure = closure;
        frame.ip = 0;
        frame.slot = self.stack_top - arg_acount - 1;

        return true;
    }

    inline fn currentFrame(self: *VM) *CallFrame {
        return &self.frames[self.frame_count - 1];
    }

    fn readByte(self: *VM) u8 {
        const frame = self.currentFrame();
        const byte = frame.closure.function.chunk.code.items[frame.ip];
        frame.ip += 1;
        return byte;
    }

    inline fn readConstant(self: *VM) Value {
        return self.currentFrame().closure.function.chunk.constants.items[self.readByte()];
    }

    /// reads two 16bits operand
    inline fn readShort(self: *VM) u16 {
        const frame = self.currentFrame();
        frame.ip += 2;
        const items = frame.closure.function.chunk.code.items;
        return (@as(u16, items[frame.ip - 2]) << 8) | items[frame.ip - 1];
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
