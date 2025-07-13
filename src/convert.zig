const std = @import("std");
const Allocator = std.mem.Allocator;
const json = std.json;
const assert = std.debug.assert;

const nyasdf = @import("nyasdf.zig");
const Data = nyasdf.Data;
const DataPackage = nyasdf.DataPackage;

const CoValue = struct {
    n: *Data,
    j: *const json.Value,

    fn set(allocator: Allocator, n: *Data, j: json.Value) Allocator.Error!void {
        n.* = switch (j) {
            .null => .{ .null = .{} },
            .bool => |b| .{ .bool = .{ .val = b } },
            .integer => |i| blk: {
                var int: Data.Int = .init(allocator);
                errdefer int.deinit();
                try int.set(i);
                break :blk .{ .int = int };
            },
            .float => |f| .{ .f64 = .{ .val = f } },
            .string, .number_string => |s| .{ .string = .{ .val = try allocator.dupeZ(u8, s) } },
            .array => |a| blk: {
                var list_val: std.ArrayListUnmanaged(*const Data) = .empty;
                try list_val.ensureTotalCapacityPrecise(allocator, a.items.len);
                break :blk .{ .list = .{ .val = list_val } };
            },
            .object => |o| blk: {
                var list_val: std.ArrayListUnmanaged(*const Data) = .empty;
                try list_val.ensureTotalCapacityPrecise(allocator, o.count());
                break :blk .{ .list = .{ .val = list_val } };
            },
        };
    }
};

const CoIterator = union(enum) {
    array: Array,
    object: Object,

    fn init(cv: CoValue, data_list: *std.ArrayList(*Data)) Allocator.Error!CoIterator {
        return switch (cv.j.*) {
            .array  => .{ .array  = try .init(cv, data_list) },
            .object => .{ .object = try .init(cv, data_list) },
            else => unreachable,
        };
    }

    fn next(self: *CoIterator) ?CoValue {
        return switch (self.*) {
            inline else => |*iter| iter.next(),
        };
    }

    const Array = struct {
        n: []const *Data,
        j: []const json.Value,
        next_idx: usize = 0,

        fn init(cv: CoValue, data_list: *std.ArrayList(*Data)) Allocator.Error!CoIterator.Array {
            const n = &cv.n.list.val;
            const j = cv.j.array;
            assert(n.capacity >= j.items.len);
            try data_list.ensureUnusedCapacity(j.items.len);
            for (j.items) |_| {
                const data = try data_list.allocator.create(Data);
                data.* = .{ .null = .{} };
                data_list.appendAssumeCapacity(data);
                n.appendAssumeCapacity(data);
            }
            return .{ .n = @ptrCast(n.items), .j = j.items };
        }

        fn next(self: *CoIterator.Array) ?CoValue {
            if (self.next_idx >= self.n.len) return null;
            const res: CoValue = .{ .n = self.n[self.next_idx], .j = &self.j[self.next_idx] };
            self.next_idx += 1;
            return res;
        }
    };

    const Object = struct {
        n: []const *const Data,
        next_idx: usize = 0,
        j: json.ObjectMap.Iterator,

        fn init(cv: CoValue, data_list: *std.ArrayList(*Data)) Allocator.Error!CoIterator.Object {
            const n = &cv.n.list.val;
            const j = cv.j.object;
            assert(n.capacity >= j.count());
            try data_list.ensureUnusedCapacity(3 * j.count());

            var iter = j.iterator();
            while (iter.next()) |entry| {
                const pair = try data_list.allocator.create(Data);
                pair.* = .{ .pair = .{ .@"0" = pair, .@"1" = pair } };
                data_list.appendAssumeCapacity(pair);
                n.appendAssumeCapacity(pair);

                const key = try data_list.allocator.create(Data);
                data_list.appendAssumeCapacity(key);
                pair.pair.@"0" = key;
                key.* = .{ .string = .{ .val = try data_list.allocator.dupeZ(u8, entry.key_ptr.*) } };

                const value = try data_list.allocator.create(Data);
                value.* = .{ .null = .{} };
                data_list.appendAssumeCapacity(value);
                pair.pair.@"1" = value;
            }

            iter.reset();
            return .{ .n = n.items, .j = iter };
        }

        fn next(self: *CoIterator.Object) ?CoValue {
            if (self.next_idx >= self.n.len) return null;
            const res: CoValue = .{ .n = @constCast(self.n[self.next_idx].pair.@"1"), .j = self.j.next().?.value_ptr };
            self.next_idx += 1;
            return res;
        }
    };
};

/// convert `std.json.Value` to nyasdf (nyas Data Format)
pub fn fromJsonValue(allocator: Allocator, value: json.Value) Allocator.Error!DataPackage {
    var pack: DataPackage = .{ .list = .init(allocator) };
    errdefer pack.deinit();

    var stack: std.ArrayListUnmanaged(CoIterator) = .empty;
    defer stack.deinit(allocator);

    try pack.list.ensureTotalCapacity(1);
    const vdata = try allocator.create(Data);
    pack.list.appendAssumeCapacity(vdata);

    try CoValue.set(allocator, vdata, value);
    switch (value) {
        .array, .object => try stack.append(allocator, try .init(.{ .n = vdata, .j = &value }, &pack.list)),
        else => {},
    }

    while (stack.items.len > 0) {
        const top = &stack.items[stack.items.len - 1];
        if (top.next()) |cv| {
            try CoValue.set(allocator, cv.n, cv.j.*);
            switch (cv.j.*) {
                .array, .object => try stack.append(allocator, try .init(cv, &pack.list)),
                else => {},
            }
        } else {
            stack.items.len -= 1;
        }
    }
    return pack;
}
