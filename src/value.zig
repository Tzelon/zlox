const std = @import("std");

const ValueType = enum {
    nil,
    boolean,
    number,
};

pub const Value = union(ValueType) {
    nil,
    boolean: bool,
    number: f64,

    pub fn fromBool(x: bool) Value {
        return Value{ .boolean = x };
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
            .nil => b == .nil,
        };
    }

    pub fn printValue(value: Value) !void {
        switch (value) {
            .number => |val| std.debug.print("{d}", .{val}),
            .nil => |val| std.debug.print("{}", .{val}),
            .boolean => |val| std.debug.print("{}", .{val}),
        }
    }
};
