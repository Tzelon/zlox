const std = @import("std");
const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;
const Allocator = std.mem.Allocator;

pub const Obj = struct {
    obj_type: Type,
    next: ?*Obj,

    pub const Type = enum { String };

    fn create(vm: *VM, comptime T: type, objtype: Type) *T {
        const prt_t = vm.allocator.create(T) catch @panic("Error creating object\n");

        prt_t.obj = Obj{ .obj_type = objtype, .next = vm.objects };
        vm.objects = &prt_t.obj;

        return prt_t;
    }

    pub inline fn isA(value: Value, objectType: Type) bool {
        return value == .obj and value.obj.obj_type == objectType;
    }

    pub fn asString(self: *Obj) *String {
        return @fieldParentPtr(String, "obj", self);
    }

    pub fn destroy(self: *Obj, vm: *VM) void {
        switch (self.obj_type) {
            .String => self.asString().destroy(vm),
        }
    }

    pub const String = struct {
        obj: Obj,
        len: usize,
        chars: []const u8,
        hash: u32,

        /// assume we cannot take ownership of the characters,
        /// so, we creates a copy of them on the heap.
        /// example are literal strings from source.
        pub fn copy(vm: *VM, chars: []const u8) *String {
            const hash = hashString(chars);

            if (vm.strings.findString(chars, hash)) |interned| return interned;

            const heap = vm.allocator.alloc(u8, chars.len) catch {
                @panic("Error copying String\n");
            };
            @memcpy(heap, chars);

            return allocate(vm, heap, hash);
        }

        /// assume we can take ownership of the characters.
        /// examples are concatenate strings
        pub fn take(vm: *VM, chars: []const u8) *String {
            const hash = hashString(chars);
            if (vm.strings.findString(chars, hash)) |interned| {
                vm.allocator.free(chars);
                return interned;
            }
            return allocate(vm, chars, hash);
        }

        fn allocate(vm: *VM, chars: []const u8, hash: u32) *String {
            const str = Obj.create(vm, String, .String);
            str.chars = chars;
            str.hash = hash;

            _ = vm.strings.set(str, Value.fromNil());

            vm.push(Value.fromObj(&str.obj));

            return str;
        }

        pub fn destroy(self: *String, vm: *VM) void {
            vm.allocator.free(self.chars);
            vm.allocator.destroy(self);
        }

        fn hashString(chars: []const u8) u32 {
            var hash: u32 = 2166136261;

            for (chars) |char| {
                hash ^= char;
                hash *%= 16777610;
            }

            return hash;
        }
    };
};
