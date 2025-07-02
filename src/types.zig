const builtin = @import("builtin");
const std = @import("std");

const nyasdf = @import("nyasdf.zig");

const Allocator = std.mem.Allocator;
const Content = nyasdf.Content;
const big_endian = builtin.cpu.arch.endian() == .big;


pub const Byte = struct {
    df_ctn: *const Content,
    byte: u8,

    pub fn init(ctn: *const Content) Byte {
        return .{
            .df_ctn = ctn,
            .byte = undefined,
        };
    }

    pub fn deinit(self: Byte) void {
        _ = self;
    }

    pub fn set(self: *Byte, val: u8) void {
        self.byte = val;
    }

    pub fn get(self: *Byte) u8 {
        return self.byte;
    }
};

pub const Bytes = struct {
    df_ctn: *const Content,
    bytes: []u8,

    pub fn init(ctn: *const Content) Bytes {
        return .{
            .df_ctn = ctn,
            .bytes = &.{},
        };
    }

    pub fn deinit(self: Bytes) void {
        const allocator = self.df_ctn.datas.allocator;
        allocator.free(self.bytes);
    }

    pub fn set(self: *Bytes, val: []const u8) Allocator.Error!void {
        const allocator = self.df_ctn.datas.allocator;
        allocator.free(self.bytes);
        errdefer self.bytes = &.{};
        self.bytes = try allocator.dupe(u8, val);
    }

    pub fn get(self: @This()) []u8 {
        return self.bytes;
    }
};

pub const String = struct {
    df_ctn: *const Content,
    string: [:0]const u8,

    pub fn init(ctn: *const Content) String {
        return .{
            .df_ctn = ctn,
            .string = empty_string,
        };
    }

    pub fn deinit(self: String) void {
        self.freeString();
    }

    pub fn set(self: *String, val: [:0]const u8) Allocator.Error!void {
        const allocator = self.df_ctn.datas.allocator;
        self.freeString();
        errdefer self.string = empty_string;
        self.string = try allocator.dupeZ(u8, val);
    }

    pub fn get(self: String) [:0]const u8 {
        return self.string;
    }

    const empty_string = "";

    fn freeString(self: String) void {
        const allocator = self.df_ctn.datas.allocator;
        if (self.string.ptr != empty_string) {
            allocator.free(self.string[0..self.string.len + 1]);
        }
    }
};

pub const Integer = struct {
    df_ctn: *const Content,
    is_negative: bool,
    full_byte_count: u16,
    extra_bit_count: u3,
    data: [*]const u8,

    pub fn init(ctn: *const Content) Integer {
        return .{
            .df_ctn = ctn,
            .is_negative = false,
            .full_byte_count = 0,
            .extra_bit_count = 0,
            .data = empty_data,
        };
    }

    pub fn deinit(self: Integer) void {
        self.freeData();
    }

    pub fn set(self: *Integer, val: anytype) Allocator.Error!void {
        const info = @typeInfo(@TypeOf(val));
        if (info != .int) @compileError(@typeName(Integer) ++ ".set only accepts integers");
        const UInt = std.meta.Int(.unsigned, info.int.bits);

        const allocator = self.df_ctn.datas.allocator;
        self.freeData();
        errdefer self.* = .init(self.df_ctn);

        const neg = val < 0; // this can determited at comptime if val is a uint
        self.is_negative = neg;
        const uint: UInt = if (neg) @bitCast(-val) else @bitCast(val);

        const bits: u16 = info.int.bits - @clz(uint);
        if (bits == 0) {
            self.data = empty_data;
            return;
        }
        self.full_byte_count = bits / 8;
        self.extra_bit_count = @truncate(bits % 8);
        const byte_count = @as(usize, self.full_byte_count) + @intFromBool(self.extra_bit_count != 0);
        const data = try allocator.alloc(u8, byte_count);
        errdefer allocator.free(data);

        const le_uint: UInt = if (big_endian) @byteSwap(uint) else uint;
        const bytes: [*]const u8 = @ptrCast(&le_uint);
        @memcpy(data, bytes);
        self.data = data.ptr;

        if (self.extra_bit_count != 0) { // clean dirty data outside the integer in last byte
            const data_mask = (@as(u8, 1) << self.extra_bit_count) - 1;
            data[byte_count - 1] &= data_mask;
        }
    }

    /// truncate if the bit size of `Int` is less than `.minFitableBits()`,
    /// always use bit cast regardless of the signedness.
    pub fn get(self: Integer, comptime Int: type) Int {
        const info = @typeInfo(Int);
        if (info != .int) @compileError(@typeName(Integer) ++ ".get only accepts integer types");
        const IntBuffer = [@sizeOf(Int)]u8;

        const byte_count = @as(usize, self.full_byte_count) + @intFromBool(self.extra_bit_count != 0);
        if (byte_count == 0) return 0;

        var int_buffer = std.mem.zeroes(IntBuffer);
        @memcpy(int_buffer[0..@min(int_buffer.len, byte_count)], self.data);
        if (big_endian) int_buffer = @byteSwap(int_buffer);

        var int: Int = @bitCast(int_buffer);
        if (self.is_negative) int = (~int) + 1; // manually perform negative operation to work around uint

        return int;
    }

    pub fn minFitableBits(self: Integer) u32 {
        var bits: u32 = self.full_byte_count;
        bits *= 8;
        bits += self.extra_bit_count;
        if (bits > 0) bits += @intFromBool(self.is_negative);
        // ? check for -2^n
        return bits;
    }

    fn freeData(self: Integer) void {
        const allocator = self.df_ctn.datas.allocator;
        const byte_count = @as(usize, self.full_byte_count) + @intFromBool(self.extra_bit_count != 0);
        allocator.free(self.data[0..byte_count]);
    }

    const empty_data: [*]const u8 = @ptrFromInt(std.math.maxInt(usize));
};

pub const F32 = struct {
    df_ctn: *const Content,
    f32: f32,

    pub fn init(ctn: *const Content) F32 {
        return .{
            .df_ctn = ctn,
            .f32 = undefined,
        };
    }

    pub fn deinit(self: F32) void {
        _ = self;
    }

    pub fn set(self: *F32, val: f32) void {
        self.f32 = val;
    }

    pub fn get(self: *F32) f32 {
        return self.f32;
    }
};

pub const F64 = struct {
    df_ctn: *const Content,
    f64: f64,

    pub fn init(ctn: *const Content) F64 {
        return .{
            .df_ctn = ctn,
            .f64 = undefined,
        };
    }

    pub fn deinit(self: F64) void {
        _ = self;
    }

    pub fn set(self: *F64, val: f64) void {
        self.f64 = val;
    }

    pub fn get(self: *F64) f64 {
        return self.f64;
    }
};

pub const Pair = struct {
    df_ctn: *const Content,
    @"0": u64,
    @"1": u64,

    pub fn init(ctn: *const Content) Pair {
        return .{
            .df_ctn = ctn,
            .@"0" = undefined,
            .@"1" = undefined,
        };
    }

    pub fn deinit(self: Pair) void {
        _ = self;
    }

    pub const SetError = error { NotInTheSameContent };

    pub fn set0(self: *Pair, data: *const nyasdf.Data) SetError!void {
        if (self.df_ctn != data.df_ctn) return SetError.NotInTheSameContent;
        self.@"0" = data.index;
    }
    pub fn set1(self: *Pair, data: *const nyasdf.Data) SetError!void {
        if (self.df_ctn != data.df_ctn) return SetError.NotInTheSameContent;
        self.@"1" = data.index;
    }
    pub fn set(self: *Pair, @"0": *const nyasdf.Data, @"1": *const nyasdf.Data) SetError!void {
        try self.set0(@"0");
        try self.set1(@"1");
    }

    pub fn get0(self: Pair) ?*nyasdf.Data {
        const datas = self.df_ctn.datas;
        if (self.@"0" >= datas.items.len) return null;
        return datas.items[self.@"0"];
    }
    pub fn get1(self: Pair) ?*nyasdf.Data {
        const datas = self.df_ctn.datas;
        if (self.@"1" >= datas.items.len) return null;
        return datas.items[self.@"1"];
    }
};

pub const List = struct {
    df_ctn: *const Content,
    indicies: std.ArrayList(u64),

    pub fn init(ctn: *const Content) List {
        return .{
            .df_ctn = ctn,
            .indicies = .init(ctn.datas.allocator),
        };
    }

    pub fn deinit(self: List) void {
        self.indicies.deinit();
    }

    pub const AppendError = error { NotInTheSameContent } || Allocator.Error;

    pub fn append(self: *List, data: *const nyasdf.Data) AppendError!void {
        if (self.df_ctn != data.df_ctn) return AppendError.NotInTheSameContent;
        try self.indicies.append(data.index);
    }

    pub fn appendSlise(self: *List, datas: []const *const nyasdf.Data) AppendError!void {
        for (datas) |data| {
            if (self.df_ctn != data.df_ctn) return AppendError.NotInTheSameContent;
        }
        try self.indicies.ensureUnusedCapacity(datas.len);
        const old_len = self.indicies.items.len;
        self.indicies.items.len += datas.len;
        const new_slice = self.indicies.items[old_len..];
        for (datas, new_slice) |data, *index| index.* = data.index;
    }

    pub fn length(self: List) usize {
        return self.indicies.items.len;
    }

    pub fn pop(self: *List) ?*nyasdf.Data {
        const datas = self.df_ctn.datas;
        const index = self.indicies.pop() orelse return null;
        if (index >= datas.items.len) return null;
        return datas.items[index];
    }

    pub fn get(self: List, index: usize) ?*nyasdf.Data {
        const datas = self.df_ctn.datas;
        if (index >= self.indicies.items.len) return null;
        const k = self.indicies.items[index];
        if (k >= datas.items.len) return null;
        return datas.items[k];
    }
};
