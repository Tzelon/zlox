const std = @import("std");
const Obj = @import("./object.zig").Obj;

pub const NAN_BOXING = true;
pub const Value = if (NAN_BOXING) NanBoxedValue else UnionValue;

const NanBoxedValue = struct {
    data: u64,

    const SIGN_BIT: u64 = 0x8000000000000000;
    const QNAN: u64 = 0x7ffc000000000000;
    const TAG_NIL = 1; // 01.
    const TAG_FALSE = 2; // 10.
    const TAG_TRUE = 3; // 11.

    const NIL_VAL = NanBoxedValue{ .data = QNAN | TAG_NIL };
    const TRUE_VAL = NanBoxedValue{ .data = QNAN | TAG_TRUE };
    const FALSE_VAL = NanBoxedValue{ .data = QNAN | TAG_FALSE };

    pub fn fromNumber(n: f64) NanBoxedValue {
        return NanBoxedValue{ .data = @bitCast(n) };
    }

    pub fn fromBool(x: bool) NanBoxedValue {
        return if (x) TRUE_VAL else FALSE_VAL;
    }

    pub fn fromObj(x: *Obj) NanBoxedValue {
        return NanBoxedValue{ .data = SIGN_BIT | QNAN | @intFromPtr(x) };
    }

    pub fn asBool(self: NanBoxedValue) bool {
        std.debug.assert(self.isBool());
        return self.data == TRUE_VAL.data;
    }

    pub fn asNumber(n: NanBoxedValue) f64 {
        std.debug.assert(n.isNumber());
        return @bitCast(n);
    }

    pub fn asObj(self: NanBoxedValue) *Obj {
        std.debug.assert(self.isObj());
        return @ptrFromInt(@as(usize, @intCast(self.data & ~(SIGN_BIT | QNAN))));
    }

    pub fn isBool(self: NanBoxedValue) bool {
        return (self.data & FALSE_VAL.data) == FALSE_VAL.data;
    }

    pub fn isNil(self: NanBoxedValue) bool {
        return self.data == NIL_VAL.data;
    }
    pub fn isObj(self: NanBoxedValue) bool {
        return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    fn isNumber(self: *NanBoxedValue) bool {
        return (self.data & QNAN) != QNAN;
    }
};

const ValueType = enum {
    nil,
    boolean,
    number,
    obj,
};

const UnionValue = union(ValueType) {
    nil,
    boolean: bool,
    number: f64,
    obj: *Obj,

    pub fn fromBool(x: bool) Value {
        return Value{ .boolean = x };
    }

    pub fn fromObj(x: *Obj) Value {
        return Value{ .obj = x };
    }

    pub fn fromNumber(x: f64) Value {
        return Value{ .number = x };
    }

    pub fn fromNil() Value {
        return Value.nil;
    }

    pub fn isA(self: Value, valueType: ValueType) bool {
        return @as(ValueType, self) == valueType;
    }

    pub fn equal(self: Value, b: Value) bool {
        return switch (self) {
            .number => b == .number and self.number == b.number,
            .boolean => b == .boolean and self.boolean == b.boolean,
            .obj => {
                if (b == .obj) {
                    return self.obj == b.obj;
                }

                return false;
            },
            .nil => b == .nil,
        };
    }

    pub fn printValue(value: Value) !void {
        switch (value) {
            .number => |val| std.debug.print("{d}", .{val}),
            .nil => |val| std.debug.print("{}", .{val}),
            .boolean => |val| std.debug.print("{}", .{val}),
            .obj => |val| switch (val.obj_type) {
                .Class => std.debug.print("{s}", .{val.asClass().name.chars}),
                .BoundMethod => printFunction(val.asBoundMethod().method.function),
                .String => std.debug.print("{s}", .{val.asString().chars}),
                .Upvalue => std.debug.print("upvalue", .{}),
                .Native => std.debug.print("<native fn>", .{}),
                .Instance => std.debug.print("{s} instance", .{val.asInstance().class.name.chars}),
                .Function => printFunction(val.asFunction()),
                .Closure => printFunction(val.asClosure().function),
            },
        }
    }

    fn printFunction(function: *Obj.Function) void {
        const name = if (function.name) |name| name.chars else "script";
        std.debug.print("<fn {s}>", .{name});
    }
};
