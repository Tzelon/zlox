const std = @import("std");
const Obj = @import("./object.zig").Obj;

const ValueType = enum {
    nil,
    boolean,
    number,
    obj,
};

pub const Value = union(ValueType) {
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
                .String => std.debug.print("{s}", .{val.asString().chars}),
                .Upvalue => std.debug.print("upvalue", .{}),
                .Native => std.debug.print("<native fn>", .{}),
                .Instance => std.debug.print("{s} instance", .{val.asInstance().class.name.chars}),
                .Function => {
                    const name = if (val.asFunction().name) |name| name.chars else "script";
                    std.debug.print("<fn {s}>", .{name});
                },
                .Closure => {
                    const name = if (val.asClosure().function.name) |name| name.chars else "script";
                    std.debug.print("<fn {s}>", .{name});
                },
            },
        }
    }
};
