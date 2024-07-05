const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const String = @import("./object.zig").Obj.String;

pub const Table = struct {
    allocator: Allocator,
    //count entries and tombstones
    count: usize,
    entries: []Entry,

    const Entry = struct { key: ?*String, value: Value };

    pub fn init(allocator: Allocator) Table {
        return Table{ .allocator = allocator, .count = 0, .entries = &[_]Entry{} };
    }

    pub fn deinit(self: *Table) void {
        self.allocator.free(self.entries);
        self.count = 0;
    }

    pub fn get(self: *Table, key: *String, value: *Value) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);

        if (entry.key == null) return false;

        value.* = entry.value;

        return true;
    }

    /// return true if this a new key
    pub fn set(self: *Table, key: *String, value: Value) bool {
        //check for 75% capacity
        if (4 * (self.count + 1) > self.entries.len * 3) {
            self.adjustCapacity();
        }

        const entry = findEntry(self.entries, key);
        const is_new_key = entry.key == null;
        if (is_new_key and entry.value.isA(.nil)) self.count += 1;

        entry.key = key;
        entry.value = value;

        return is_new_key;
    }

    pub fn findString(self: *Table, chars: []const u8, hash: u32) ?*String {
        if (self.count == 0) return null;
        var index = hash & (self.entries.len - 1);

        while (true) {
            const entry = &self.entries[index];

            if (entry.key) |key| {
                if (key.hash == hash and
                    key.chars.len == chars.len and
                    std.mem.eql(u8, chars, key.chars))
                {
                    return key;
                }
            } else if (entry.value.isA(.nil)) {
                //stop if we find an empty non-tomebstone entry.
                return null;
            }

            index = (index + 1) & (self.entries.len - 1);
        }
    }

    pub fn delete(self: *Table, key: *String) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);

        if (entry.key == null) return false;

        entry.key = null;
        entry.value = Value.fromBool(true);

        return true;
    }

    fn findEntry(entries: []Entry, key: *String) *Entry {
        var index = key.hash & (entries.len - 1);
        var tombstone: ?*Entry = null;

        while (true) {
            const entry = &entries[index];
            if (entry.key == null) {
                if (entry.value.isA(.nil)) {
                    //empty entry.
                    return tombstone orelse entry;
                } else {
                    //we found a tombstone.
                    if (tombstone == null) tombstone = entry;
                }
            } else if (entry.key == key) {
                // we found the key.
                return entry;
            }

            index = (index + 1) & (entries.len - 1);
        }
    }

    fn adjustCapacity(self: *Table) void {
        const new_capacity = if (self.entries.len < 8) 8 else self.entries.len * 2;
        //create new array of entries and init them.
        const entries = self.allocator.alloc(Entry, new_capacity) catch @panic("Cannot allocate new capacity\n");
        for (entries) |*e| {
            e.key = null;
            e.value = Value.nil;
        }

        //re-insert the old entries to the new entries array, skipping tombstones.
        self.count = 0;
        for (self.entries) |entry| {
            if (entry.key == null) continue;
            const dest = findEntry(entries, entry.key.?);

            dest.key = entry.key;
            dest.value = entry.value;
            self.count += 1;
        }

        //free old entries
        self.allocator.free(self.entries);

        self.entries = entries;
    }

    /// copy entries from one hashtable into another.
    pub fn addAll(self: *Table, to: *Table) void {
        for (self.entries) |entry| {
            if (entry.key != null) {
                _ = to.set(entry.key.?, entry.value);
            }
        }
    }
};

test "create Table" {
    const VM = @import("./vm.zig").VM;

    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var vm = VM.init(allocator);
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    const key = String.copy(&vm, "name");
    const val = Value.fromNumber(1.9);

    try expect(table.set(key, val) == true);
    var val_2 = Value.fromNil();
    try expect(table.get(key, &val_2));
    try expect(val_2.equal(val));
}
