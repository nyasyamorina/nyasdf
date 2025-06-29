const builtin = @import("builtin");
const std = @import("std");
const File = std.fs.File;

const utils = @import("utils.zig");


pub const Manager = struct {
    file: File,

    pub fn init(file: File) @This() {
        return .{ .file = file };
    }

    pub fn deinit(self: @This()) void {
        _ = self;
    }

    pub fn entry(self: @This(), offset: u64) Entry {
        return .{ .file = self.file, .offset = offset };
    }
};


pub const Entry = struct {
    file: File,
    offset: u64,

    pub const SaveIntError = error {
        IntTooBig,
    } || File.SeekError || File.WriteError;

    pub fn saveInt(self: @This(), int: anytype) SaveIntError!u64 {
        const Int = @TypeOf(int);
        comptime std.debug.assert(@typeInfo(Int) == .int);
        const int_bits = @typeInfo(Int).int.bits;
        const expand_bytes = int_bits / 8 + (if (int_bits % 8 == 0) 0 else 1);
        const expand_bits = 8 * expand_bytes;
        const ExpandInt = std.meta.Int(@typeInfo(Int).int.signedness, expand_bits);
        const Bytes = [expand_bytes]u8;

        const neg = int < 0;
        var expand_int: ExpandInt = if (neg) -int else int;
        var sl: utils.SignedIntLength = .{ .length = 0, .negative = neg };

        const max_bits = 8 * std.math.maxInt(u7);
        if (expand_bits > max_bits) {
            const high_mask = ~@as(ExpandInt, std.math.maxInt(std.meta.Int(.unsigned, max_bits)));
            if (expand_int & high_mask != 0) return SaveIntError.IntTooBig;
        }

        const byte_count_unknown = expand_bytes - @divTrunc(@clz(expand_int), 8);
        std.debug.assert(byte_count_unknown <= std.math.maxInt(u7));
        sl.length = if (@typeInfo(@TypeOf(byte_count_unknown)).int.bits > 7)
            @truncate(byte_count_unknown)
        else
            byte_count_unknown
        ;

        if (builtin.cpu.arch.endian() == .big) expand_int = @byteSwap(expand_int);

        const header: [2]u8 = .{ @intFromEnum(DataType.int), @bitCast(sl)};
        const bytes: Bytes = @bitCast(expand_int);

        try self.file.seekTo(self.offset);
        try self.file.writeAll(&header);
        try self.file.writeAll(bytes[0..sl.length]);

        return sl.length + header.len;
    }

    pub const LoadDataTypeError = utils.ReadExactError || File.SeekError;

    pub fn loadDataType(self: @This()) LoadDataTypeError!DataType {
        try self.file.seekTo(self.offset);
        var dtype: DataType = .invalid;
        try utils.readExact(self.file, @ptrCast((&dtype)[0..1]));
        return dtype;
    }

    pub const LoadIntError = error {
        WrongDataType,
        NegativeNeedSignedInteger,
        IntTooBig,
    } || utils.ReadExactError || File.SeekError;

    pub fn loadInt(self: @This(), comptime Int: type, truncate: bool) LoadIntError!Int {
        comptime std.debug.assert(@typeInfo(Int) == .int);
        const int_bits = @typeInfo(Int).int.bits;
        const expand_bytes = int_bits / 8 + (if (int_bits % 8 == 0) 0 else 1);
        const expand_bits = 8 * expand_bytes;
        const ext_bits: u3 = expand_bits - @typeInfo(Int).int.bits;
        const signed = @typeInfo(Int).int.signedness == .signed;
        const ExpandInt = std.meta.Int(@typeInfo(Int).int.signedness, expand_bits);
        const Bytes = [expand_bytes]u8;

        if (builtin.mode == .Debug and try self.loadDataType() != .int) return LoadIntError.WrongDataType;

        try self.file.seekTo(self.offset + @sizeOf(DataType));
        var sl: utils.SignedIntLength = undefined;
        try utils.readExact(self.file, @ptrCast((&sl)[0..1]));
        if (!signed and sl.negative) return LoadIntError.NegativeNeedSignedInteger;

        var bytes = std.mem.zeroes(Bytes);
        if (sl.length > bytes.len) {
            if (!truncate) return LoadIntError.IntTooBig;
            try utils.readExact(self.file, &bytes);
        } else {
            try utils.readExact(self.file, bytes[0..sl.length]);
            if (ext_bits != 0 and !truncate) {
                const ext_mask = (@as(u8, 1) << ext_bits) - 1;
                if (bytes[bytes.len - 1] & ext_mask != 0) return LoadIntError.IntTooBig;
            }
        }

        var expand_int: ExpandInt = @bitCast(bytes);
        if (builtin.cpu.arch.endian() == .big) expand_int = @byteSwap(expand_int);
        if (signed and sl.negative) expand_int = -expand_int;

        return @truncate(expand_int);
    }
};


pub const DataType = enum(u8) {
    int,
    f32,
    f64,
    pair,
    list,
    array,
    string,
    _,

    pub const invalid: DataType = @enumFromInt(std.math.maxInt(u8));
};
