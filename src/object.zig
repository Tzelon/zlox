const std = @import("std");
const Value = @import("./value.zig").Value;
const Table = @import("./table.zig").Table;
const Chunk = @import("./chunk.zig").Chunk;
const VM = @import("./vm.zig").VM;
const Allocator = std.mem.Allocator;

pub const Obj = struct {
    obj_type: Type,
    next: ?*Obj,
    is_mark: bool = false,

    pub const Type = enum { String, Function, Native, Closure, Upvalue, Class, Instance, BoundMethod };

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
        return @fieldParentPtr("obj", self);
    }

    pub fn asFunction(self: *Obj) *Function {
        return @fieldParentPtr("obj", self);
    }

    pub fn asNative(self: *Obj) *Native {
        return @fieldParentPtr("obj", self);
    }

    pub fn asClosure(self: *Obj) *Closure {
        return @fieldParentPtr("obj", self);
    }

    pub fn asUpvalue(self: *Obj) *Upvalue {
        return @fieldParentPtr("obj", self);
    }

    pub fn asClass(self: *Obj) *Class {
        return @fieldParentPtr("obj", self);
    }

    pub fn asInstance(self: *Obj) *Instance {
        return @fieldParentPtr("obj", self);
    }

    pub fn asBoundMethod(self: *Obj) *BoundMethod {
        return @fieldParentPtr("obj", self);
    }

    pub fn destroy(self: *Obj, vm: *VM) void {
        switch (self.obj_type) {
            .String => self.asString().destroy(vm),
            .Function => self.asFunction().destroy(vm),
            .Native => self.asNative().destroy(vm),
            .Closure => self.asClosure().destroy(vm),
            .Upvalue => self.asUpvalue().destroy(vm),
            .Class => self.asClass().destroy(vm),
            .Instance => self.asInstance().destroy(vm),
            .BoundMethod => self.asBoundMethod().destroy(vm),
        }
    }

    pub const Upvalue = struct {
        obj: Obj,
        location: *Value,
        closed: Value = undefined,
        next: ?*Upvalue,

        pub fn create(vm: *VM, slot: *Value) *Upvalue {
            const upvalue = Obj.create(vm, Upvalue, .Upvalue);
            upvalue.location = slot;
            upvalue.next = null;

            return upvalue;
        }

        pub fn destroy(self: *Upvalue, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Function = struct {
        obj: Obj,
        arity: u8,
        chunk: Chunk,
        name: ?*Obj.String,
        upvalue_count: u8,

        pub fn create(vm: *VM) *Function {
            const func = Obj.create(vm, Function, .Function);
            func.arity = 0;
            func.chunk = Chunk.init(vm.allocator);
            func.name = null;
            func.upvalue_count = 0;

            return func;
        }

        pub fn destroy(self: *Function, vm: *VM) void {
            self.chunk.deinit();
            vm.allocator.destroy(self);
        }
    };

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

            vm.push(Value.fromObj(&str.obj));
            _ = vm.strings.set(str, Value.fromNil());
            _ = vm.pop();

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

    pub const Native = struct {
        obj: Obj,
        function: NativeFn,

        pub const NativeFn = *const fn (args: []Value) Value;

        pub fn create(vm: *VM, func: NativeFn) *Native {
            const native = Obj.create(vm, Native, .Native);
            native.function = func;

            return native;
        }

        pub fn destroy(self: *Native, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Closure = struct {
        obj: Obj,
        function: *Function,
        upvalue_count: u8,
        upvalues: []*Upvalue,

        pub const NativeFn = *const fn (args: []Value) Value;

        pub fn create(vm: *VM, func: *Function) *Closure {
            const closure = Obj.create(vm, Closure, .Closure);

            closure.function = func;
            closure.upvalues = vm.allocator.alloc(*Upvalue, func.upvalue_count) catch @panic("Error creating Closure Upvalues");
            closure.upvalue_count = func.upvalue_count;

            return closure;
        }

        pub fn destroy(self: *Closure, vm: *VM) void {
            vm.allocator.free(self.upvalues);
            // NOTE: we only free the Closure not the Function.
            // That’s because the closure doesn’t own the function.
            vm.allocator.destroy(self);
        }
    };

    pub const Class = struct {
        obj: Obj,
        name: *String,
        methods: Table,

        pub fn create(vm: *VM, name: *String) *Class {
            const class = Obj.create(vm, Class, .Class);

            class.name = name;
            class.methods = Table.init(vm.allocator);

            return class;
        }

        pub fn destroy(self: *Class, vm: *VM) void {
            self.methods.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const BoundMethod = struct {
        obj: Obj,
        method: *Closure,
        receiver: Value,

        pub fn create(vm: *VM, method: *Closure, receiver: Value) *BoundMethod {
            const bound_method = Obj.create(vm, BoundMethod, .BoundMethod);

            bound_method.method = method;
            bound_method.receiver = receiver;

            return bound_method;
        }

        pub fn destroy(self: *BoundMethod, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Instance = struct {
        obj: Obj,
        class: *Class,
        fields: Table,

        pub fn create(vm: *VM, class: *Class) *Instance {
            const instance = Obj.create(vm, Instance, .Instance);

            instance.class = class;
            instance.fields = Table.init(vm.allocator);

            return instance;
        }

        pub fn destroy(self: *Instance, vm: *VM) void {
            self.fields.deinit();
            vm.allocator.destroy(self);
        }
    };
};
