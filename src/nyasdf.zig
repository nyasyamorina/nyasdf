const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

/// used to indicate the entry of nyasdf (nyas Data Format)
pub const entry_label = "nyasdf\x06\x07";


pub const Data = union(Data.Type) {
    // value types
    null:   Data.Null,
    bool:   Data.Bool,
    byte:   Data.Byte,
    f16:    Data.F16,
    f32:    Data.F32,
    f64:    Data.F64,
    bytes:  Data.Bytes,
    string: Data.String,
    int:    Data.Int,
    // ref types
    pair:   Data.Pair,
    list:   Data.List,

    pub const Type = enum(u8) {
        // value types
        null = 0x00,
        bool,
        byte,
        f16,
        f32,
        f64,
        bytes,
        string,
        int,
        // ref types
        pair = 0x80,
        list,

        pub fn isRef(self: Data.Type) bool {
            return @intFromEnum(self) & 0x80 != 0;
        }
    };

    pub fn isRef(self: Data) bool {
        return @as(Data.Type, self).isRef();
    }

    pub fn updateHash(self: Data, hasher: anytype) void {
        const tag: Data.Type = self;
        hasher.update(std.mem.asBytes(&tag));

        switch (self) {
            inline else => |payload| {
                payload.updateHash(hasher);
            },
        }
    }

    pub fn eql(self: Data, other: Data) bool {
        if (@as(Data.Type, self) != @as(Data.Type, other)) return false;

        return switch (self) { // TODO: rewrite using `inline else`
            .null   => |d| d.eql(other.null  ),
            .bool   => |d| d.eql(other.bool  ),
            .byte   => |d| d.eql(other.byte  ),
            .f16    => |d| d.eql(other.f16   ),
            .f32    => |d| d.eql(other.f32   ),
            .f64    => |d| d.eql(other.f64   ),
            .bytes  => |d| d.eql(other.bytes ),
            .string => |d| d.eql(other.string),
            .int    => |d| d.eql(other.int   ),
            .pair   => |d| d.eql(other.pair  ),
            .list   => |d| d.eql(other.list  ),
        };
    }

    /// automatically call `deinit` based on tagged type
    pub fn deinit(self: Data) void {
        switch (self) {
            inline else => |payload| {
                const Payload = @TypeOf(payload);
                if (std.meta.hasFn(Payload, "deinit")) payload.deinit();
            },
        }
    }

    pub const WriteValueIntoError = error { GotRefData };

    /// write value data using custom writer, make sure writer has method `writer.write([]const u8) WriteError!usize`,
    /// return `WriteValueIntoError.GotRefType` if the data is not a value.
    ///
    /// check out `WriteIterator` if want to write a array of data as `nyasdf` (nyas data format).
    pub fn writeValueInto(self: Data, writer: anytype, comptime WriteError: type) (WriteError || WriteValueIntoError)!usize {
        if (self.isRef()) return WriteValueIntoError.GotRefData;
        return self.writeValueIntoAssumeNotRef(writer, WriteError);
    }

    /// write value data using custom writer, make sure writer has method `writer.write([]const u8) WriteError!usize`,
    /// assume the data is not a ref.
    ///
    /// check out `WriteIterator` if want to write a array of data as `nyasdf` (nyas data format).
    pub fn writeValueIntoAssumeNotRef(self: Data, writer: anytype, comptime WriteError: type) WriteError!usize {
        assert(!self.isRef());
        const tag: u8 = @intFromEnum(self);
        const tag_len = try writer.write((&tag)[0..1]);

        switch (self) {
            inline else => |payload| {
                if (std.meta.hasFn(@TypeOf(payload), "writeInto")) {
                    const payload_len = try payload.writeInto(writer, WriteError);
                    return tag_len + payload_len;
                } else unreachable;
            },
        }
    }

    /// read value data using the **correct** type tag, reader position should be just after the type tag,
    /// make sure reader has method `reader.read([]u8) ReadError!usize`, assume the type tag and the data to be read are not ref.
    ///
    /// check out `ReadIterator` if want to read `nyasdf` (nyas data format) from stream.
    pub fn readValueFromAllocWithTagAssumeNotRef(
        gpa: Allocator,
        tag: Data.Type,
        reader: anytype,
        comptime ReadError: type,
    ) (GetReadExpectError(ReadError) || Allocator.Error)!Data {
        assert(!tag.isRef());
        return switch (tag) {
            .null   => .{ .null   = try .readFrom(reader, ReadError) },
            .bool   => .{ .bool   = try .readFrom(reader, ReadError) },
            .byte   => .{ .byte   = try .readFrom(reader, ReadError) },
            .f16    => .{ .f16    = try .readFrom(reader, ReadError) },
            .f32    => .{ .f32    = try .readFrom(reader, ReadError) },
            .f64    => .{ .f64    = try .readFrom(reader, ReadError) },
            .bytes  => .{ .bytes  = try .readFromAlloc(gpa, reader, ReadError) },
            .string => .{ .string = try .readFromAlloc(gpa, reader, ReadError) },
            .int    => .{ .int    = try .readFromAlloc(gpa, reader, ReadError) },
            else    => unreachable,
        };
    }

    pub const Null = struct {
        pub inline fn updateHash(_: Data.Null, _: anytype) void {}

        pub inline fn eql(_: Data.Null, _: Data.Null) bool {
            return true;
        }

        pub inline fn writeInto(_: Data.Null, _: anytype, comptime _: type) error{}!usize {
            return 0;
        }

        pub inline fn readFrom(_: anytype, comptime _: type) error{}!Data.Null {
            return .{};
        }
    };

    pub const Bool = struct {
        val: bool,

        pub fn updateHash(self: Data.Bool, hasher: anytype) void {
            const i = @intFromBool(self.val);
            hasher.update(std.mem.asBytes(&i)[0..1]);
        }

        pub fn eql(self: Data.Bool, other: Data.Bool) bool {
            return self.val == other.val;
        }

        pub fn writeInto(self: Data.Bool, writer: anytype, comptime WriteError: type) WriteError!usize {
            const i = @intFromBool(self.val);
            return writer.write(std.mem.asBytes(&i)[0..1]);
        }

        pub fn readFrom(reader: anytype, comptime ReadError: type) GetReadExpectError(ReadError)!Data.Bool {
            var i: u8 = 0;
            try readExpectFrom(reader, (&i)[0..1], ReadError);
            return .{ .val = i & 1 != 0 };
        }
    };

    pub const Byte = struct {
        val: u8,

        pub fn updateHash(self: Data.Byte, hasher: anytype) void {
            hasher.update(std.mem.asBytes(&self.val));
        }

        pub fn eql(self: Data.Byte, other: Data.Byte) bool {
            return self.val == other.val;
        }

        pub fn writeInto(self: Data.Byte, writer: anytype, comptime WriteError: type) WriteError!usize {
            return writer.write(std.mem.asBytes(&self.val));
        }

        pub fn readFrom(reader: anytype, comptime ReadError: type) GetReadExpectError(ReadError)!Data.Byte {
            var self: Data.Byte = .{ .val = undefined };
            try readExpectFrom(reader, std.mem.asBytes(&self.val), ReadError);
            return self;
        }
    };

    pub const F16 = struct {
        val: f16,

        pub fn updateHash(self: Data.F16, hasher: anytype) void {
            hasher.update(std.mem.asBytes(&self.val));
        }

        pub fn eql(self: Data.F16, other: Data.F16) bool {
            return @as(u16, @bitCast(self.val)) == @as(u16, @bitCast(other.val));
        }

        pub fn writeInto(self: Data.F16, writer: anytype, comptime WriteError: type) WriteError!usize {
            return writer.write(std.mem.asBytes(&self.val));
        }

        pub fn readFrom(reader: anytype, comptime ReadError: type) GetReadExpectError(ReadError)!Data.F16 {
            var self: Data.F16 = .{ .val = undefined };
            try readExpectFrom(reader, std.mem.asBytes(&self.val), ReadError);
            return self;
        }
    };

    pub const F32 = struct {
        val: f32,

        pub fn updateHash(self: Data.F32, hasher: anytype) void {
            hasher.update(std.mem.asBytes(&self.val));
        }

        pub fn eql(self: Data.F32, other: Data.F32) bool {
            return @as(u32, @bitCast(self.val)) == @as(u32, @bitCast(other.val));
        }

        pub fn writeInto(self: Data.F32, writer: anytype, comptime WriteError: type) WriteError!usize {
            return writer.write(std.mem.asBytes(&self.val));
        }

        pub fn readFrom(reader: anytype, comptime ReadError: type) GetReadExpectError(ReadError)!Data.F32 {
            var self: Data.F32 = .{ .val = undefined };
            try readExpectFrom(reader, std.mem.asBytes(&self.val), ReadError);
            return self;
        }
    };

    pub const F64 = struct {
        val: f64,

        pub fn updateHash(self: Data.F64, hasher: anytype) void {
            hasher.update(std.mem.asBytes(&self.val));
        }

        pub fn eql(self: Data.F64, other: Data.F64) bool {
            return @as(u64, @bitCast(self.val)) == @as(u64, @bitCast(other.val));
        }

        pub fn writeInto(self: Data.F64, writer: anytype, comptime WriteError: type) WriteError!usize {
            return writer.write(std.mem.asBytes(&self.val));
        }

        pub fn readFrom(reader: anytype, comptime ReadError: type) GetReadExpectError(ReadError)!Data.F64 {
            var self: Data.F64 = .{ .val = undefined };
            try readExpectFrom(reader, std.mem.asBytes(&self.val), ReadError);
            return self;
        }
    };

    pub const Bytes = struct {
        val: []const u8,

        pub fn updateHash(self: Data.Bytes, hasher: anytype) void {
            hasher.update(self.val);
        }

        pub fn eql(self: Data.Bytes, other: Data.Bytes) bool {
            return std.mem.eql(u8, self.val, other.val);
        }

        pub fn writeInto(self: Data.Bytes, writer: anytype, comptime WriteError: type) WriteError!usize {
            const len_count = try writeVarIntInto(writer, self.val.len, WriteError);
            const byte_count = try writer.write(self.val);
            return len_count + byte_count;
        }

        pub fn readFromAlloc(
            gpa: Allocator,
            reader: anytype,
            comptime ReadError: type,
        ) (GetReadExpectError(ReadError) || Allocator.Error)!Data.Bytes {
            const len: usize = @intCast(try readVarIntFrom(reader, ReadError));
            const self: Data.Bytes = .{ .val = try gpa.alloc(u8, len) };
            errdefer gpa.free(self.val);
            try readExpectFrom(reader, @constCast(self.val), ReadError);
            return self;
        }
    };

    pub const String = struct {
        val: [:0]const u8,

        pub fn updateHash(self: Data.String, hasher: anytype) void {
            hasher.update(self.val);
        }

        pub fn eql(self: Data.String, other: Data.String) bool {
            return std.mem.eql(u8, self.val, other.val);
        }

        pub fn writeInto(self: Data.String, writer: anytype, comptime WriteError: type) WriteError!usize {
            const len_count = try writeVarIntInto(writer, self.val.len, WriteError);
            const byte_count = try writer.write(self.val);
            return len_count + byte_count;
        }

        pub fn readFromAlloc(
            gpa: Allocator,
            reader: anytype,
            comptime ReadError: type,
        ) (GetReadExpectError(ReadError) || Allocator.Error)!Data.String {
            const len: usize = @intCast(try readVarIntFrom(reader, ReadError));
            const self: Data.String = .{ .val = try gpa.allocSentinel(u8, len, 0) };
            errdefer gpa.free(self.val);
            try readExpectFrom(reader, @constCast(self.val), ReadError);
            return self;
        }
    };

    pub const Int = struct {
        /// user should not manipulate this
        buffer: std.ArrayList(u8),
        negative: bool = false,

        pub fn init(gpa: Allocator) Data.Int {
            return .{ .buffer = .init(gpa) };
        }

        pub fn deinit(self: Data.Int) void {
            self.buffer.deinit();
        }

        pub fn updateHash(self: Data.Int, hasher: anytype) void {
            const n = @intFromBool(self.negative);
            hasher.update(std.mem.asBytes(&n)[0..1]);
            hasher.update(self.buffer.items);
        }

        pub fn eql(self: Data.Int, other: Data.Int) bool {
            if (self.negative != other.negative) return false;
            return std.mem.eql(u8, self.buffer.items, other.buffer.items);
        }

        pub fn writeInto(self: Data.Int, writer: anytype, comptime WriteError: type) WriteError!usize {
            const len_neg = (self.buffer.items.len << 1) | @intFromBool(self.negative);
            const len_neg_count = try writeVarIntInto(writer, len_neg, WriteError);
            const byte_count = try writer.write(self.buffer.items);
            return len_neg_count + byte_count;
        }

        pub fn readFromAlloc(
            gpa: Allocator,
            reader: anytype,
            comptime ReadError: type,
        ) (GetReadExpectError(ReadError) || Allocator.Error)!Data.Int {
            const len_neg: usize = @intCast(try readVarIntFrom(reader, ReadError));
            var self: Data.Int = .init(gpa);
            errdefer self.deinit();
            self.negative = len_neg & 1 != 0;
            try self.buffer.resize(len_neg >> 1);
            try readExpectFrom(reader, self.buffer.items, ReadError);
            return self;
        }

        pub fn set(self: *Data.Int, int: anytype) Allocator.Error!void {
            const info = @typeInfo(@TypeOf(int));
            if (info != .int) @compileError(@typeName(Data.Int) ++ " only accepts integers.");
            const is_signed = info.int.signedness == .signed;
            const bit_count = info.int.bits;
            const UInt = std.meta.Int(.unsigned, bit_count);

            const is_neg = int < 0; // can be determined in comptime if int is a unsigned integer
            self.negative = is_neg;
            const uint: UInt = if (is_signed) @bitCast(if (is_neg) -%int else int) else int;

            const byte_count = std.math.divCeil(u16, bit_count - @clz(uint), 8) catch unreachable;
            try self.buffer.resize(byte_count);
            @memcpy(self.buffer.items, std.mem.asBytes(&uint)[0..byte_count]);
        }

        /// apply `bitCast` and `truncate` automatically
        pub fn get(self: Data.Int, comptime I: type) I {
            if (@typeInfo(I) != .int) @compileError(@typeName(Data.Int) ++ " only accepts integers.");

            var int = std.mem.zeroes(I);
            const bytes = std.mem.asBytes(&int);
            const byte_count = @min(bytes.len, self.buffer.items.len);
            @memcpy(bytes[0..byte_count], self.buffer.items[0..byte_count]);

            return if (self.negative) -%int else int;
        }

        /// the bit count that can fit the integer without truncation.
        pub fn minFitableBits(self: Data.Int) u16 {
            if (self.buffer.items.len == 0) return 0;
            const max_int_byte_count = comptime std.math.divCeil(u16, std.math.maxInt(u16), 8) catch unreachable;
            assert(self.buffer.items.len <= max_int_byte_count);
            assert(self.buffer.getLast() != 0);
            const last_byte_leading_zero_bit_count = @clz(self.buffer.getLast());
            var bit_count = 8 * self.buffer.items.len - last_byte_leading_zero_bit_count + @intFromBool(self.negative);
            // check for negative power of 2
            if (self.negative and std.math.isPowerOfTwo(self.buffer.getLast())) {
                for (self.buffer.items[0 .. self.buffer.items.len - 1]) |byte| {
                    if (byte != 0) break;
                } else bit_count -= 1;
            }
            assert(bit_count <= std.math.maxInt(u16));
            return @truncate(bit_count);
        }
    };

    pub const Pair = struct {
        @"0": *const Data,
        @"1": *const Data,

        pub fn updateHash(self: Data.Pair, hasher: anytype) void {
            hasher.update(std.mem.asBytes(&self.@"0"));
            hasher.update(std.mem.asBytes(&self.@"1"));
        }

        pub fn eql(self: Data.Pair, other: Data.Pair) bool {
            return self.@"0" == other.@"0" and self.@"1" == other.@"1";
        }
    };

    pub const List = struct {
        val: std.ArrayListUnmanaged(*const Data) = .empty,

        pub fn updateHash(self: Data.List, hasher: anytype) void {
            for (self.val.items) |data| hasher.update(std.mem.asBytes(&data));
        }

        pub fn eql(self: Data.List, other: Data.List) bool {
            return std.mem.eql(*const Data, self.val.items, other.val.items);
        }
    };
};


/// a warpper of `std.ArrayList`, use this `deinit` to free data list instead of `list.deinit`
pub const DataPackage = struct {
    list: std.ArrayList(*Data),

    pub fn deinit(self: DataPackage) void {
        destroyAllData(self.list.allocator, self.list.items);
        self.list.deinit();
    }

    /// connect to `std.json.parseFromValueLeaky`
    pub fn jsonParseFromValue(allocator: Allocator, source: std.json.Value, _: std.json.ParseOptions) Allocator.Error!DataPackage {
        return convert.fromJsonValue(allocator, source);
    }

    /// connect to `std.json.parseFromTokenSourceLeaky`
    pub fn jsonParse(allocator: Allocator, source: anytype, opts: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!DataPackage {
        return convert.fromJsonTokenSource(allocator, source, opts);
    }
};

/// collect all referenced data began with `entry_data`, you must ensure all data (pointers) all valid.
/// the `entry_data` will be the first elements in returned list.
pub fn collectDataSliceAlloc(gpa: Allocator, entry_data: *const Data) Allocator.Error![]*const Data {
    if (!entry_data.isRef()) {
        const list = try gpa.alloc(*const Data, 1);
        list[0] = @constCast(entry_data);
        return list;
    }

    var collected: DataPtrMap(void) = .empty;
    defer collected.deinit(gpa);
    {
        var stack: std.ArrayListUnmanaged(*const Data) = .empty;
        defer stack.deinit(gpa);
        try stack.append(gpa, entry_data);

        while (stack.pop()) |data| {
            const e = try collected.getOrPut(gpa, @constCast(data));
            if (e.found_existing or !data.isRef()) continue;
            switch (data.*) {
                .pair => |p| try stack.appendSlice(gpa, &.{ p.@"0", p.@"1" }),
                .list => |l| try stack.appendSlice(gpa, l.val.items),
                else => unreachable,
            }
        }
    }

    var list: std.ArrayListUnmanaged(*const Data) = .empty;
    errdefer list.deinit(gpa);
    try list.ensureTotalCapacityPrecise(gpa, collected.count());

    _ = collected.remove(@constCast(entry_data));
    list.appendAssumeCapacity(entry_data);
    var iter = collected.keyIterator();
    while (iter.next()) |data| list.appendAssumeCapacity(data.*);

    assert(list.items.len == list.capacity);
    const result = list.items;
    list = .empty;
    return result;
}


pub const DataHashContext = struct {
    pub fn hash(_: DataHashContext, data: Data) u64 {
        var hasher: std.hash.Wyhash = .init(0);
        data.updateHash(&hasher);
        return hasher.final();
    }

    pub fn eql(_: DataHashContext, data1: Data, data2: Data) bool {
        return data1.eql(data2);
    }
};

/// reduce duplicate data for given data slice.
///
/// for fine control, call `reduceStart` first then `reduceContinue` looply,
/// return false if no duplicate data was found.
/// all non-duplicate data are moved to beginning of the slice given in `reduceStart`,
/// and its length is stored in `keeping_count`.
///
/// see also `reduceDirect` for direct reducion without fine control.
pub const Reducer = struct {
    allocator: Allocator,
    slice_indices: DataPtrMap(usize) = .empty,
    cached_hashes: DataPtrMap(u64) = .empty,
    refs: DataPtrMap(void) = .empty,
    data_set: DataSet = .empty,
    replaces: DataPtrMap(*Data) = .empty,
    /// the data slice given in `reduceStart`,
    /// the beginning part `0..keep_count` should not be changed during reduction.
    slice: []*Data,
    /// the non-duplicate data count after reduction
    keeping_count: usize = 0,

    const CachedDataHashContext = struct {
        hashes: *DataPtrMap(u64),

        pub fn hash(self: CachedDataHashContext, data: *Data) u64 {
            const result = self.hashes.getOrPutAssumeCapacity(data);
            if (!result.found_existing) result.value_ptr.* = DataHashContext.hash(undefined, data.*);
            return result.value_ptr.*;
        }
        pub fn eql(_: CachedDataHashContext, data1: *Data, data2: *Data) bool {
            return DataHashContext.eql(undefined, data1.*, data2.*);
        }
    };
    const DataSet = std.HashMapUnmanaged(*Data, void, CachedDataHashContext, std.hash_map.default_max_load_percentage);

    pub fn init(gpa: Allocator) Reducer {
        return .{ .allocator = gpa, .slice = &.{} };
    }

    pub fn deinit(self: *Reducer) void {
        self.slice_indices.deinit(self.allocator);
        self.cached_hashes.deinit(self.allocator);
        self.refs.deinit(self.allocator);
        self.data_set.deinit(self.allocator);
        self.replaces.deinit(self.allocator);
    }

    /// start to reduce the given data slice, return true if any data was reduced.
    /// call `reduceContinue` for more reduction after this step.
    pub fn reduceStart(self: *Reducer, slice: []*Data) Allocator.Error!bool {
        assert(slice.len <= std.math.maxInt(u32));
        const data_set_ctx: CachedDataHashContext = .{ .hashes = &self.cached_hashes };
        self.slice = slice;
        self.keeping_count = slice.len;
        if (self.keeping_count == 0) return false;

        self.keeping_count = slice.len;
        self.slice_indices.clearRetainingCapacity();
        try self.slice_indices.ensureTotalCapacity(self.allocator, @truncate(slice.len));
        self.cached_hashes.clearRetainingCapacity();
        try self.cached_hashes.ensureTotalCapacity(self.allocator, @truncate(slice.len));
        self.refs.clearRetainingCapacity();
        self.data_set.clearRetainingCapacity();
        try self.data_set.ensureTotalCapacityContext(self.allocator, @truncate(slice.len), data_set_ctx);
        self.replaces.clearRetainingCapacity();

        for (slice, 0..) |data, index| {
            self.slice_indices.putAssumeCapacityNoClobber(data, index);
            if (data.isRef()) try self.refs.putNoClobber(self.allocator, data, undefined);

            const result = self.data_set.getOrPutAssumeCapacityContext(data, data_set_ctx);
            if (result.found_existing) {
                try self.replaces.put(self.allocator, data, result.key_ptr.*);
            }
        }

        if (self.replaces.count() == 0) return false;
        self.applyReplacesToRefs();
        self.rearrangeSlice();
        return true;
    }

    /// continue to reduce data slice given in `reduceStart`, return true if any data was reduced.
    /// should call this step looply until no data was reduced or reached to maximum reducion steps.
    ///
    /// note the beginning part `0..keep_count` of the slice should not be changed during reduction.
    pub fn reduceContinue(self: *Reducer) Allocator.Error!bool {
        const data_set_ctx: CachedDataHashContext = .{ .hashes = &self.cached_hashes };
        if (self.keeping_count == 0) return false;

        self.data_set.clearRetainingCapacity();
        self.replaces.clearRetainingCapacity();

        var ref_iter = self.refs.keyIterator();
        while (ref_iter.next()) |p_ref| {
            const result = self.data_set.getOrPutAssumeCapacityContext(p_ref.*, data_set_ctx);
            if (result.found_existing) {
                try self.replaces.put(self.allocator, p_ref.*, result.key_ptr.*);
            }
        }

        if (self.replaces.count() == 0) return false;
        self.applyReplacesToRefs();
        self.rearrangeSlice();
        return true;
    }

    /// the data slice after reduction, this is the beginning part of the data slice given in `reduceStart`.
    /// data outside of this slice can be safely freed (assume all referenced data are also in the original slice).
    pub fn keepingSlice(self: Reducer) []const *Data {
        return self.slice[0..self.keeping_count];
    }

    fn applyReplacesToRefs(self: *Reducer) void {
        // remove duplicate ref data in self.refs
        var replace_iter = self.replaces.keyIterator();
        while (replace_iter.next()) |p_replaced_data| {
            _ = self.refs.remove(p_replaced_data.*);
        }
        // apply replaces to ref data
        var ref_iter = self.refs.keyIterator();
        while (ref_iter.next()) |p_ref| {
            var changed = false;
            switch (p_ref.*.*) {
                .pair => |*p| {
                    changed |= self.applyReplaces(&p.@"0");
                    changed |= self.applyReplaces(&p.@"1");
                },
                .list => |l| {
                    for (l.val.items) |*data| changed |= self.applyReplaces(data);
                },
                else => unreachable,
            }
            if (changed) _ = self.cached_hashes.remove(p_ref.*);
        }
    }

    fn applyReplaces(self: Reducer, p_data: **const Data) bool {
        if (self.replaces.get(@constCast(p_data.*))) |replace_to| {
            p_data.* = replace_to;
            return true;
        }
        return false;
    }

    fn rearrangeSlice(self: *Reducer) void {
        var replace_iter = self.replaces.keyIterator();
        while (replace_iter.next()) |p_replaced_data| {
            self.keeping_count -= 1;
            const replaced_index = self.slice_indices.get(p_replaced_data.*).?;
            std.mem.swap(*Data, &self.slice[replaced_index], &self.slice[self.keeping_count]);
            self.slice_indices.putAssumeCapacity(p_replaced_data.*, self.keeping_count);
            self.slice_indices.putAssumeCapacity(self.slice[replaced_index], replaced_index);
        }
    }

    /// one line reduction for someone who don't care fine control and don't want to write template code over again and again,
    /// such as me.
    ///
    /// return the non-duplicate data count after reduction, these data are at the beginning of the slice,
    /// data after these can be safely freed (assume all referenced data are also in the original slice).
    pub fn reduceDirect(gpa: Allocator, slice: []*Data, max_reduction_count: ?u32) Allocator.Error!usize {
        if (max_reduction_count != null and max_reduction_count == 0) return slice.len;
        var count_down = max_reduction_count orelse std.math.maxInt(u32);

        var self: Reducer = .init(gpa);
        defer self.deinit();

        if (try self.reduceStart(slice)) {
            count_down -= 1;
            while (count_down > 0 and try self.reduceContinue()) : (count_down -= 1) {}
        }

        return self.keeping_count;
    }
};


/// write nyasdf (nyas Data Format) into something, such as into file (see `FileWriteIterator`).
///
/// for fine control, use `setDataSlice` to set the data slice to write, optionly call `ensureDataValid` to validfy data,
/// then call `writeNext` looply to write data util null is returned indicating that all data has been written,
/// and finally call `writePosTable` and `writeEntry` to complete the nyasdf write.
///
/// see also `writeDirect` for direct writing without fine control.
pub fn WriteIterator(
    comptime Context: type,
    comptime WriteErrorSet: type,
    comptime writeFn: fn (ctx: Context, bytes: []const u8) WriteErrorSet!usize,
    comptime GetSeekPosErrorSet: type,
    comptime getPosFn: fn (ctx: Context) GetSeekPosErrorSet!u64,
) type {
    return struct {
        context: Context,
        allocator: Allocator,
        /// available after `writePosTable`
        table_pos: ?u64 = null,
        pos_table: std.ArrayListUnmanaged(u64) = .empty,
        slice_indices: DataPtrMap(usize) = .empty,
        slice: []const *const Data = &.{},
        next_index: usize = 0,
        stage: Stage = .inited,

        /// just for fun, you can even write something like this:
        /// ```zig
        /// const data_slice: []*const Data = ...;
        /// var w_iter: WriteIterator = .init(ally, ctx);
        /// defer w_iter.deinit()
        ///
        /// while (true) {
        ///     switch(w_iter.stage) {
        ///         .inited => try w_iter.setDataSlice(data_slice);
        ///         .write_next => try w_iter.writeNext(),
        ///         .write_pos_table => try w_iter.writePosTable(),
        ///         .write_entry => try w_iter.writeEntry(),
        ///         .done => break,
        ///     }
        /// }
        /// ```
        pub const Stage = enum {
            inited,
            write_next,
            write_pos_table,
            write_entry,
            done,
            deinited,
        };

        pub const WriteError = WriteErrorSet;
        pub inline fn write(self: @This(), bytes: []const u8) WriteError!usize {
            return writeFn(self.context, bytes);
        }
        pub const GetSeekPosError = GetSeekPosErrorSet;
        pub inline fn getPos(self: @This()) GetSeekPosError!u64 {
            return getPosFn(self.context);
        }
        pub const GetSeekPosOrWriteError = GetSeekPosError || WriteError;

        pub const InternalWriter = std.io.GenericWriter(Context, WriteErrorSet, writeFn);
        pub inline fn internalWriter(self: @This()) InternalWriter {
            return .{ .context = self.context };
        }

        pub fn init(gpa: Allocator, write_ctx: Context) @This() {
            return .{ .context = write_ctx, .allocator = gpa };
        }
        /// should call `context.deinit()` manually if any.
        pub fn deinit(self: *@This()) void {
            self.stage = .deinited;
            self.pos_table.deinit(self.allocator);
            self.slice_indices.deinit(self.allocator);
        }

        /// set the data slice to be written, all referenced data must be in this slice.
        pub fn setDataSlice(self: *@This(), slice: []const *const Data) Allocator.Error!void {
            // stage: .inited, .done
            assert(slice.len <= std.math.maxInt(u32));
            self.slice = slice;
            self.next_index = 0;

            self.pos_table.clearRetainingCapacity();
            try self.pos_table.ensureTotalCapacityPrecise(self.allocator, slice.len);

            self.slice_indices.clearRetainingCapacity();
            try self.slice_indices.ensureTotalCapacity(self.allocator, @truncate(slice.len));
            for (slice, 0..) |data, index| {
                self.slice_indices.putAssumeCapacity(@constCast(data), index);
            }

            self.stage = .write_next;
        }

        pub const InvalidDataError = error {
            /// one or more ref data that refers to data is not contained in the given data slice
            ReferencedDataNotInSlice,
        };
        /// ensure all data can be written. see `InvalidDataError` for possible invalid conditions.
        ///
        /// can be safely write data by using `writeNextAssumeValid` after this step.
        pub fn ensureDataValid(self: @This()) InvalidDataError!void {
            // stage: .write_next
            for (self.slice) |data| {
                // currently only ref data needs to check its validation
                if (data.isRef() and !self.checkRefDataValid(data.*))
                    return InvalidDataError.RefedDataNotInSlice;
            }
        }
        fn checkRefDataValid(self: @This(), ref: Data) bool {
            switch (ref) {
                .pair => |p| {
                    return self.contains(p.@"0") and self.contains(p.@"1");
                },
                .list => |l| {
                    return for (l.val.items) |d| {
                        if (!self.contains(d)) break false;
                    } else true;
                },
                else => unreachable,
            }
        }

        /// check the data slice contains the given data or not.
        pub fn contains(self: @This(), data: *const Data) bool {
            return self.slice_indices.contains(@constCast(data));
        }

        pub const NextWriteError = GetSeekPosOrWriteError || InvalidDataError || Allocator.Error;
        /// write next data, return null if all data were written.
        pub fn writeNext(self: *@This()) NextWriteError!?usize {
            // stage: .write_next
            if (self.next_index >= self.slice.len) {
                self.stage = .write_pos_table;
                return null;
            }

            const seek_pos = try self.getPos();
            const data = self.slice[self.next_index];
            const count = switch (data.*) {
                .pair => |p| try self.writePair(p),
                .list => |l| try self.writeList(l),
                else => blk: {
                    if (builtin.mode == .Debug) assert(!data.isRef());
                    break :blk try data.writeValueIntoAssumeNotRef(self, WriteError);
                },
            };

            try self.pos_table.append(self.allocator, seek_pos); // user may manipulated `slice` after `setDataSlice`
            self.next_index += 1;
            return count;
        }
        /// write next data, return null if all data were written.
        /// you should call `ensureDataValid` to ensure all data is valid.
        pub fn writeNextAssumeValid(self: *@This()) GetSeekPosOrWriteError!?usize {
            if (self.next_index >= self.slice.len) return null;

            const seek_pos = try self.getPos();
            const data = self.slice[self.next_index];
            const count = switch (self.slice[self.next_index]) {
                .pair => |p| try self.writePairAssumeValid(p),
                .list => |l| try self.writeListAssumeValid(l),
                else => {
                    if (builtin.mode == .Debug) assert(!data.isRef());
                    try data.writeValueIntoAssumeNotRef(self, WriteError);
                },
            };

            self.pos_table.appendAssumeCapacity(seek_pos);
            self.next_index += 1;
            return count;
        }

        fn writePair(self: @This(), pair: Data.Pair) (InvalidDataError || WriteError)!usize {
            const tag: u8 = @intFromEnum(Data.Type.pair);
            var count = try self.write((&tag)[0..1]);
            count += try self.writeRefedIndex(pair.@"0");
            count += try self.writeRefedIndex(pair.@"1");
            return count;
        }
        fn writePairAssumeValid(self: @This(), pair: Data.Pair) WriteError!usize {
            const tag: u8 = @intFromEnum(Data.Type.pair);
            var count = try self.write((&tag)[0..1]);
            count += try self.writeRefedIndexAssumeValid(pair.@"0");
            count += try self.writeRefedIndexAssumeValid(pair.@"1");
            return count;
        }
        fn writeList(self: @This(), list: Data.List) (InvalidDataError || WriteError)!usize {
            const tag: u8 = @intFromEnum(Data.Type.list);
            var count = try self.write((&tag)[0..1]);
            count += try self.writeVarInt(list.val.items.len);
            for (list.val.items) |data| count += try self.writeRefedIndex(data);
            return count;
        }
        fn writeListAssumeValid(self: @This(), list: Data.List) (InvalidDataError || WriteError)!usize {
            const tag: u8 = @intFromEnum(Data.Type.list);
            var count = try self.write((&tag)[0..1]);
            count += try self.writeVarInt(list.val.items.len);
            for (list.val.items) |data| count += try self.writeRefedIndexAssumeValid(data);
            return count;
        }

        fn writeRefedIndex(self: @This(), data: *const Data) (InvalidDataError || WriteError)!usize {
            const idx = self.slice_indices.get(@constCast(data)) orelse
                return InvalidDataError.ReferencedDataNotInSlice;
            return self.writeVarInt(idx);
        }
        fn writeRefedIndexAssumeValid(self: @This(), data: *const Data) WriteError!usize {
            const idx = self.slice_indices.get(@constCast(data)).?;
            return self.writeVarInt(idx);
        }

        /// write the data position table, this method should be called after all data were written.
        pub fn writePosTable(self: *@This()) GetSeekPosOrWriteError!usize {
            // stage: .write_pos_table
            self.table_pos = try self.getPos();
            var count: usize = 0;
            for (self.pos_table.items) |pos| count += try self.writeVarInt(pos);
            self.stage = .write_entry;
            return count;
        }
        /// write the entry of nyasdf (nyas Data Format), this method shold be called after `writePositionTable`.
        pub fn writeEntry(self: *@This()) WriteError!usize {
            // stage: .write_entry
            assert(self.table_pos != null);
            var count = try self.write(entry_label);
            count += try self.writeVarInt(self.table_pos.?);
            count += try self.writeVarInt(self.pos_table.items.len);
            self.stage = .done;
            return count;
        }

        fn writeVarInt(self: @This(), int: u64) WriteError!usize {
            return writeVarIntInto(self, int, WriteError);
        }

        pub const WriteDirectError = NextWriteError || Allocator.Error;
        /// directly write nyasdf (nyas Data Format) into `write_ctx` without fine control,
        /// all referenced data must be in the given slice.
        pub fn writeDirect(gpa: Allocator, write_ctx: Context, slice: []const *const Data) WriteDirectError!usize {
            var self: @This() = .init(gpa, write_ctx);
            defer self.deinit();
            try self.setDataSlice(slice);

            var count: usize = 0;
            while (try self.writeNext()) |c| count += c;
            count += try self.writePosTable();
            count += try self.writeEntry();

            return count;
        }
    };
}

pub const FileWriteIterator = WriteIterator(
    std.fs.File,
    std.fs.File.WriteError,
    std.fs.File.write,
    std.fs.File.GetSeekPosError,
    std.fs.File.getPos,
);


/// read nyasdf (nyas Data Format) from something, such as from file (see `FileReadIterator`).
///
/// fine control for reading is not so meaningful as writing, see `readDirect` for direct redaing.
///
/// elements in the result data list can be destroyed using `destroyAllData`.
pub fn ReadIterator(
    comptime Context: type,
    comptime ReadErrorSet: type,
    comptime readFn: fn (ctx: Context, buffer: []u8) ReadErrorSet!usize,
    comptime SeekErrorSet: type,
    comptime seekToFn: fn (ctx: Context, pos: u64) SeekErrorSet!void,
    comptime GetSeekPosErrorSet: type,
    comptime getEndPosFn: fn (ctx: Context) GetSeekPosErrorSet!u64,
) type {
    return struct {
        context: Context,
        allocator: Allocator,
        /// available after `findEntry` if entry was found
        entry_pos: ?u64 = null,
        /// available after `readEntry`
        table_pos: ?u64 = null,
        /// available after `readEntry`
        data_count: usize = 0,
        /// available after `readPosTable`
        pos_table: std.ArrayListUnmanaged(u64) = .empty,
        refs: std.ArrayListUnmanaged(IndexRefDataWithInfo) = .empty,
        data_list: std.ArrayListUnmanaged(*Data) = .empty,
        next_index: usize = 0,

        pub const ReadError = ReadErrorSet;
        pub inline fn read(self: @This(), buffer: []u8) ReadError!usize {
            return readFn(self.context, buffer);
        }
        pub const SeekError = SeekErrorSet;
        pub inline fn seekTo(self: @This(), pos: u64) SeekError!void {
            return seekToFn(self.context, pos);
        }
        pub const GetSeekPosError = GetSeekPosErrorSet;
        pub inline fn getEndPos(self: @This()) GetSeekPosError!u64 {
            return getEndPosFn(self.context);
        }
        pub const ReadExpectError = GetReadExpectError(ReadError);
        pub const SeekOrReadExpectError = SeekError || ReadExpectError;

        pub const InternalReader = std.io.GenericReader(Context, ReadErrorSet, readFn);
        pub inline fn internalReader(self: @This()) InternalReader {
            return .{ .context = self.context };
        }

        pub fn init(gpa: Allocator, read_ctx: Context) @This() {
            return .{ .context = read_ctx, .allocator = gpa };
        }
        pub fn deinit(self: *@This()) void {
            self.destroyRefAndData();
            self.pos_table.deinit(self.allocator);
            self.refs.deinit(self.allocator);
            self.data_list.deinit(self.allocator);
        }

        fn destroyRefAndData(self: @This()) void {
            for (self.refs.items) |ref| {
                switch (ref.data) {
                    .pair => {},
                    .list => |l| self.allocator.free(l.val.allocatedSlice()),
                }
            }
            destroyAllData(self.allocator, self.data_list.items);
        }

        pub const FindEntryError = GetSeekPosError || SeekOrReadExpectError || Allocator.Error || error { EntryNotFound };
        pub fn findEntry(self: *@This(), start: ?u64, length: usize) FindEntryError!void {
            const end = try self.getEndPos();
            const s_start = if (start) |s| @min(end, s) else if (end > length) end - length else 0;
            const s_length: usize = @min(length, end - s_start);
            if (s_length < entry_label.len) return FindEntryError.EntryNotFound;

            const max_buffer_len = 4096; // = chunk_len + inherit_len

            self.entry_pos = if (s_length < max_buffer_len) blk: {
                const buffer = try self.allocator.alloc(u8, s_length);
                defer self.allocator.free(buffer);

                try self.seekTo(s_start);
                try self.readExpect(buffer);

                const idx = std.mem.lastIndexOf(u8, buffer, entry_label) orelse return FindEntryError.EntryNotFound;
                break :blk s_start + idx;
            } else blk: { // search entry label by chunk from end to start
                const inherit_len = entry_label.len - 1;
                const chunk_len = max_buffer_len - inherit_len;

                var remain_search_len = s_length;
                var chunk_pos = s_start + s_length;

                const buffer = try self.allocator.alloc(u8, max_buffer_len);
                defer self.allocator.free(buffer);
                const chunk = buffer[0..chunk_len];
                const from_prev = buffer[chunk_len..];
                const to_next = buffer[0..inherit_len];
                for (from_prev) |*byte| byte.* = 0;

                var curr_chunk_len: usize = undefined;
                while (remain_search_len > 0) : (remain_search_len -= curr_chunk_len) {
                    curr_chunk_len = @min(chunk_len, remain_search_len);
                    const curr_chunk = chunk[(chunk_len - curr_chunk_len)..];

                    chunk_pos -= curr_chunk_len;
                    try self.seekTo(chunk_pos);
                    try self.readExpect(curr_chunk);

                    const search_range = curr_chunk[0..(curr_chunk_len + inherit_len)];
                    if (std.mem.lastIndexOf(u8, search_range, entry_label)) |idx| break :blk chunk_pos + idx;

                    @memcpy(from_prev, to_next);
                } else return FindEntryError.EntryNotFound;
            };
        }

        pub fn readEntry(self: *@This()) SeekOrReadExpectError!void {
            try self.seekTo(self.entry_pos.? + entry_label.len);
            self.table_pos = try self.readVarInt();
            self.data_count = @intCast(try self.readVarInt());
        }

        pub const ReadPosTableError = SeekOrReadExpectError || Allocator.Error;
        pub fn readPosTable(self: *@This()) ReadPosTableError!void {
            self.destroyRefAndData();
            self.pos_table.clearRetainingCapacity();
            try self.pos_table.ensureTotalCapacityPrecise(self.allocator, self.data_count);
            self.pos_table.items.len = self.data_count;
            self.data_list.clearRetainingCapacity();
            try self.data_list.ensureTotalCapacityPrecise(self.allocator, self.data_count);
            self.refs.clearRetainingCapacity();

            try self.seekTo(self.table_pos.?);
            for (self.pos_table.items) |*pos| {
                pos.* = try self.readVarInt();
            }
        }

        pub const ReadNextEror = SeekOrReadExpectError || Allocator.Error || error { InvalidDataTypeTag };
        pub fn readNext(self: *@This()) ReadNextEror!bool {
            if (self.next_index >= self.pos_table.items.len) return false;

            const data = try self.allocator.create(Data);
            errdefer self.allocator.destroy(data);
            try self.data_list.append(self.allocator, data); // user may manipulated `pos_table` after `readPosTable`
            errdefer _ = self.data_list.pop();
            data.* = .{ .null = .{} };

            const pos = self.pos_table.items[self.next_index];
            try self.seekTo(pos);

            var tag_byte: u8 = 0xFF;
            try self.readExpect((&tag_byte)[0..1]);
            const tag = std.enums.fromInt(Data.Type, tag_byte) orelse return ReadNextEror.InvalidDataTypeTag;
            switch (tag) {
                .pair => try self.readPair(),
                .list => try self.readList(),
                else => {
                    if (builtin.mode == .Debug) assert(!tag.isRef());
                    data.* = try .readValueFromAllocWithTagAssumeNotRef(self.allocator, tag, self, ReadError);
                },
            }

            try self.resolveRefData();
            self.next_index += 1;
            return true;
        }

        fn readPair(self: *@This()) (SeekOrReadExpectError || Allocator.Error)!void {
            const idx0: usize = @intCast(try self.readVarInt());
            const idx1: usize = @intCast(try self.readVarInt());
            try self.insertRef(.{
                .max_index = @max(idx0, idx1),
                .index_in_data_list = self.next_index,
                .data = .{ .pair = .{ .@"0" = idx0, .@"1" = idx1 } },
            });
        }
        fn readList(self: *@This()) (SeekOrReadExpectError || Allocator.Error)!void {
            const list_len: usize = @intCast(try self.readVarInt());
            var ref: IndexRefData = .{ .list = .{} };
            try ref.list.val.resize(self.allocator, list_len);
            errdefer ref.list.val.deinit(self.allocator);

            var max_index: usize = 0;
            for (ref.list.val.items) |*index| {
                index.* = @intCast(try self.readVarInt());
                max_index = @max(max_index, index.*);
            }

            try self.insertRef(.{
                .max_index = max_index,
                .index_in_data_list = self.next_index,
                .data = ref,
            });
        }
        fn insertRef(self: *@This(), ref_info: IndexRefDataWithInfo) Allocator.Error!void {
            const insert_index = for (self.refs.items, 0..) |info, index| {
                if (info.max_index <= ref_info.max_index) break index;
            } else self.refs.items.len;
            try self.refs.insert(self.allocator, insert_index, ref_info);
        }

        fn resolveRefData(self: *@This()) Allocator.Error!void {
            while (true) {
                if (self.refs.items.len == 0) break;
                var ref_info = self.refs.getLast();
                if (ref_info.max_index > self.next_index) break;

                self.data_list.items[ref_info.index_in_data_list].* = switch (ref_info.data) {
                    .pair => |p| .{ .pair = .{
                        .@"0" = self.data_list.items[p.@"0"],
                        .@"1" = self.data_list.items[p.@"1"],
                    } },

                    .list => |*l| list: {
                        var ref: Data = .{ .list = .{} };
                        try ref.list.val.ensureTotalCapacityPrecise(self.allocator, l.val.items.len);
                        errdefer ref.list.val.deinit(self.allocator);
                        ref.list.val.items.len = l.val.items.len;

                        for (ref.list.val.items, l.val.items) |*r, i| r.* = self.data_list.items[i];
                        l.val.deinit(self.allocator);

                        break :list ref;
                    },
                };
                self.refs.items.len -= 1;
            }
        }

        pub const TakeDataError = error {
            /// ref data that refers to data not in this nyasdf (nyas Data Format)
            ReferencedDataNotFound,
        };
        /// take the result data package. return `ResolveRefDataFailed` if some ref data cannot be resolved.
        ///
        /// see also `takeDataListAssumeSuccess`.
        pub fn takeData(self: *@This()) TakeDataError!DataPackage {
            if (self.refs.items.len > 0) return TakeDataError.ReferencedDataNotFound;
            return self.takeDataAssumeSuccess();
        }
        /// take the result data package. assume all data is resolved,
        /// data that cannot be resolved will be replaced by `Data.Null`.
        pub fn takeDataAssumeSuccess(self: *@This()) DataPackage {
            const data_list = self.data_list.toManaged(self.allocator);
            self.data_list = .empty;
            return .{ .list = data_list };
        }

        fn readVarInt(self: @This()) ReadExpectError!u64 {
            return readVarIntFrom(self, ReadError);
        }
        fn readExpect(self: @This(), buffer: []u8) ReadExpectError!void {
            return readExpectFrom(self, buffer, ReadError);
        }

        pub const InitDirectError = error {
            EntryNotFound,
            InvalidDataTypeTag,
        } || GetSeekPosError || SeekOrReadExpectError || Allocator.Error;
        pub fn initDirect(gpa: Allocator, read_ctx: Context) InitDirectError!@This() {
            var self: @This() = .init(gpa, read_ctx);
            errdefer self.deinit();

            try self.findEntry(null, 4096);
            try self.readEntry();
            try self.readPosTable();
            while (try self.readNext()) {}

            return self;
        }

        pub const ReadDirectError = InitDirectError || TakeDataError;
        /// directly read nyasdf (nyas Data Format)
        pub fn readDirect(gpa: Allocator, read_ctx: Context) ReadDirectError!DataPackage {
            var self: @This() = try .initDirect(gpa, read_ctx);
            defer self.deinit();
            return self.takeData();
        }

        pub const ReadDirectAssumeSuccessError = InitDirectError;
        /// directly read nyasdf (nyas Data Format), assume all data resolved succefully,
        /// data that cannot be resolved will be replaced by `Data.Null`.
        pub fn readDirectAssumeSuccess(gpa: Allocator, read_ctx: Context) ReadDirectAssumeSuccessError!DataPackage {
            var self: @This() = try .initDirect(gpa, read_ctx);
            defer self.deinit();
            return self.takeDataAssumeSuccess();
        }
    };
}

pub const FileReadIterator = ReadIterator(
    std.fs.File,
    std.fs.File.ReadError,
    std.fs.File.read,
    std.fs.File.SeekError,
    std.fs.File.seekTo,
    std.fs.File.GetSeekPosError,
    std.fs.File.getEndPos,
);


fn writeVarIntInto(writer: anytype, int: u64, comptime WriteError: type) WriteError!usize {
    var mask: u64 = 0x7F;
    var count: u6 = 1;
    while ((int & (~mask)) != 0) : (count += 1) {
        mask = (mask << 7) | 0x7F;
    }
    const result = count;
    while (count > 1) : (count -= 1) {
        const shift = 7 * (count - 1);
        const byte: u8 = @truncate((int >> shift) | 0x80);
        _ = try writer.write((&byte)[0..1]);
    }
    const byte: u8 = @truncate(int & 0x7F);
    _ = try writer.write((&byte)[0..1]);
    return result;
}
fn readVarIntFrom(reader: anytype, comptime ReadError: type) GetReadExpectError(ReadError)!u64 {
    var b: u8 = undefined;
    try readExpectFrom(reader, (&b)[0..1], ReadError);
    var i: u64 = b & 0x7F;

    while (b & 0x80 != 0) {
        try readExpectFrom(reader, (&b)[0..1], ReadError);
        i = (i << 7) | (b & 0x7F);
    }
    return i;
}

pub fn GetReadExpectError(comptime ReadError: type) type {
    return ReadError || error { EndOfStream };
}
fn readExpectFrom(reader: anytype, buffer: []u8, comptime ReadError: type) GetReadExpectError(ReadError)!void {
    const read = try reader.read(buffer);
    if (read != buffer.len) return error.EndOfStream;
}

const IndexRefData = union(enum) {
    pair: IndexRefData.Pair,
    list: IndexRefData.List,

    const Pair = struct {
        @"0": usize,
        @"1": usize,
    };

    const List = struct {
        val: std.ArrayListUnmanaged(usize) = .empty,
    };
};

const IndexRefDataWithInfo = struct {
    max_index: usize,
    index_in_data_list: usize,
    data: IndexRefData,
};

fn destroyAllData(gpa: Allocator, slice: []const *Data) void {
    for (slice) |data| {
        switch (data.*) {
            .null, .bool, .byte, .f16, .f32, .f64, .pair => {},
            .int => |i| i.deinit(),
            .list => |*l| l.val.deinit(gpa),
            .bytes => |b| gpa.free(b.val),
            .string => |s| gpa.free(s.val),
        }
        gpa.destroy(data);
    }
}

fn DataPtrMap(comptime T: type) type {
    return std.HashMapUnmanaged(*Data, T, struct {
        pub fn hash(_: @This(), data: *Data) u64 {
            return @intFromPtr(data); // extream performance, but what's the cost?
        }
        pub fn eql(_: @This(), data1: *Data, data2: *Data) bool {
            return data1 == data2;
        }
    }, std.hash_map.default_max_load_percentage);
}


pub const convert = @import("convert.zig");
