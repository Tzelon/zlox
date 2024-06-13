const std = @import("std");
const Obj = @import("./object.zig").Obj;
const VM = @import("./vm.zig").VM;
const Table = @import("./table.zig").Table;
const Value = @import("./value.zig").Value;
const Compiler = @import("./compiler.zig").Compiler;
const debug_stress_gc = @import("./debug.zig").debug_stress_gc;
const debug_log_gc = @import("./debug.zig").debug_log_gc;
const Allocator = std.mem.Allocator;

pub const GCAllocator = struct {
    parent_allocator: Allocator,
    vm: ?*VM = null,

    pub fn init(parent_allocator: Allocator) GCAllocator {
        return .{
            .parent_allocator = parent_allocator,
        };
    }

    pub fn allocator(self: *GCAllocator) Allocator {
        return .{ .ptr = self, .vtable = &.{ .alloc = alloc, .resize = resize, .free = free } };
    }

    pub fn enableGC(self: *GCAllocator, vm: *VM) void {
        self.vm = vm;
    }

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        if (debug_log_gc) {
            // std.log.debug("[GC] allocate {d} for {d}\n", .{ len, ret_addr });
        }

        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        return self.parent_allocator.rawAlloc(len, ptr_align, ret_addr);
    }

    fn resize(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));

        if (new_len > buf.len) {
            if (debug_stress_gc) {
                self.collectGarbage();
            }
        }

        return self.parent_allocator.rawResize(buf, log2_buf_align, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        if (debug_log_gc) {
            // std.log.debug("[GC] free {d} for {d}\n", .{ buf.len, ret_addr });
        }
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        return self.parent_allocator.rawFree(buf, buf_align, ret_addr);
    }

    pub fn collectGarbage(self: *GCAllocator) void {
        if (debug_log_gc) {
            std.log.debug("[GC] begin\n", .{});
        }
        self.markRoots();
        self.traceReferences();
        // self.tableRemoveWhite();
        self.sweep();

        if (debug_log_gc) {
            std.log.debug("[GC] end\n", .{});
        }
    }

    fn markRoots(self: *GCAllocator) void {
        var i: usize = 0;

        //mark values
        while (i < self.vm.?.stack_top) : (i += 1) {
            self.markValue(&self.vm.?.stack[i]);
        }

        // mark globals
        self.markTable(&self.vm.?.globals);

        // mark call frames
        i = 0;
        while (i < self.vm.?.frame_count) : (i += 1) {
            self.markObject(&self.vm.?.frames[i].closure.obj);
        }

        // mark Upvalues
        var upvalue = self.vm.?.open_upvalues;
        while (upvalue != null) : (upvalue = upvalue.?.next) {
            self.markObject(&upvalue.?.obj);
        }

        self.markCompilerRoots();
    }

    fn markValue(self: *GCAllocator, value: *Value) void {
        switch (value.*) {
            .obj => self.markObject(value.obj),
            .nil, .boolean, .number => return,
        }
    }

    fn blackenObject(self: *GCAllocator, object: *Obj) void {
        if (debug_log_gc) {
            std.log.debug("[GC] {any} blacken", .{object});
        }

        switch (object.obj_type) {
            .Upvalue => self.markValue(&Obj.asUpvalue(object).closed),
            .Function => {
                const function = Obj.asFunction(object);
                if (function.name) |name| self.markObject(&name.obj);
                self.markArray(&function.chunk.constants);
            },
            .Closure => {
                const closure = Obj.asClosure(object);
                self.markObject(&closure.function.obj);

                var i: usize = 0;
                while (i < closure.upvalue_count) : (i += 1) {
                    self.markObject(&closure.upvalues[i].obj);
                }
            },
            .Native, .String => return,
        }
    }
    fn markArray(self: *GCAllocator, array: *std.ArrayList(Value)) void {
        for (array.items) |*val| {
            self.markValue(val);
        }
    }

    fn markObject(self: *GCAllocator, object: *Obj) void {
        if (object.is_mark) return;
        if (debug_log_gc) {
            std.debug.print("debug: {*} ---  ", .{object});
            Value.printValue(Value.fromObj(object)) catch unreachable;
            std.debug.print(" ---  \n", .{});
        }
        object.is_mark = true;

        self.vm.?.gray_stack.append(object) catch unreachable;
    }

    fn traceReferences(self: *GCAllocator) void {
        while (self.vm.?.gray_stack.items.len > 0) {
            const object = self.vm.?.gray_stack.pop();

            self.blackenObject(object);
        }
    }

    fn sweep(self: *GCAllocator) void {
        var prev: ?*Obj = null;
        var object = self.vm.?.objects;
        while (object) |obj| {
            if (obj.is_mark) {
                obj.is_mark = false;
                prev = obj;
                object = obj.next;
            } else {
                const unreached = obj;
                if (prev) |p| {
                    p.next = obj;
                } else {
                    self.vm.?.objects = obj;
                }
                std.debug.print("unreachable?! {} \n", .{unreached});
                unreached.destroy(self.vm.?);
            }
        }
    }

    fn tableRemoveWhite(self: *GCAllocator) void {
        var table = self.vm.?.strings;
        var i: usize = 0;
        while (i < table.count) : (i += 1) {
            const entry = &table.entries[i];
            if (entry.key != null and !entry.key.?.obj.is_mark) {
                _ = table.delete(entry.key.?);
            }
        }
    }

    fn markTable(self: *GCAllocator, table: *Table) void {
        var i: usize = 0;
        while (i < table.count) : (i += 1) {
            const entry = &table.entries[i];
            if (entry.key != null) {
                self.markObject(&entry.key.?.obj);
            }
            self.markValue(&entry.value);
        }
    }

    fn markCompilerRoots(self: *GCAllocator) void {
        var compiler: ?*Compiler = self.vm.?.parser.?.compiler;

        while (compiler) |_| : (compiler = compiler.?.enclosing) {
            self.markObject(&compiler.?.function.obj);
        }
    }
};
