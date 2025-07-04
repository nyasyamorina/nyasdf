const builtin = @import("builtin");
const std = @import("std");

const Allocator = std.mem.Allocator;
const File = std.fs.File;
const big_endian = builtin.cpu.arch.endian() == .big;


pub const DataFormat = struct {
    datas: std.ArrayList(*Data),

    pub const entry_lable = "nyasdf";

    pub fn init(allocator: Allocator) DataFormat {
        return .{ .datas = .init(allocator) };
    }

    pub fn deinit(self: DataFormat) void {
        for (self.datas.items) |data| {
            data.payload.deinit();
            self.datas.allocator.destroy(data);
        }
        self.datas.deinit();
    }

    pub fn newData(self: *DataFormat, @"type": Data.Type) Allocator.Error!*Data {
        const new_data = try self.datas.allocator.create(Data);
        errdefer self.datas.allocator.destroy(new_data);
        try self.datas.append(new_data);

        new_data.* = .{
            .df = self,
            .index = self.datas.items.len - 1,
            .payload = try .init(@"type", self),
        };
        return new_data;
    }

    pub fn newByte(self: *DataFormat) Allocator.Error!*Data.Payload.Byte {
        const data = try self.newData(.byte);
        return &data.payload.byte;
    }
    pub fn newBytes(self: *DataFormat) Allocator.Error!*Data.Payload.Bytes {
        const data = try self.newData(.bytes);
        return &data.payload.bytes;
    }
    pub fn newString(self: *DataFormat) Allocator.Error!*Data.Payload.String {
        const data = try self.newData(.string);
        return &data.payload.string;
    }
    pub fn newInteger(self: *DataFormat) Allocator.Error!*Data.Payload.Integer {
        const data = try self.newData(.integer);
        return &data.payload.integer;
    }
    pub fn newF32(self: *DataFormat) Allocator.Error!*Data.Payload.F32 {
        const data = try self.newData(.f32);
        return &data.payload.f32;
    }
    pub fn newF64(self: *DataFormat) Allocator.Error!*Data.Payload.F64 {
        const data = try self.newData(.f64);
        return &data.payload.f64;
    }
    pub fn newPair(self: *DataFormat) Allocator.Error!*Data.Payload.Pair {
        const data = try self.newData(.pair);
        return &data.payload.pair;
    }
    pub fn newList(self: *DataFormat) Allocator.Error!*Data.Payload.List {
        const data = try self.newData(.list);
        return &data.payload.list;
    }

    pub fn countDatas(self: DataFormat) usize {
        return self.datas.items.len;
    }

    /// useful for checking data validation after reducing
    pub fn containData(self: DataFormat, data: *const Data) bool {
        for (self.datas.items) |data2| {
            if (data == data2) return true;
        } else return false;
    }

    pub fn getData(self: DataFormat, index: usize) ?*Data {
        if (index >= self.datas.items.len) return null;
        return self.datas.items[index];
    }

    /// remove duplicate datas, data will be invalid after this step.
    pub fn reduce(self: *DataFormat) Allocator.Error!void {
        const allocator = self.datas.allocator;

        var ref_datas: std.ArrayList(*Data) = .init(allocator);
        defer ref_datas.deinit();

        const DataHashContext = struct {
            pub fn hash(_: @This(), data: *const Data) u64 {
                return Data.Payload.HashContext.hash(undefined, data.payload);
            }
            pub fn eql(_: @This(), data1: *const Data, data2: *const Data) bool {
                return Data.Payload.HashContext.eql(undefined, data1.payload, data2.payload);
            }
        };
        var uni_datas: std.HashMap(*const Data, void, DataHashContext, std.hash_map.default_max_load_percentage) = .init(allocator);
        defer uni_datas.deinit();

        var reduces: std.ArrayList(struct{ from: u64, to: u64 }) = .init(allocator);
        defer reduces.deinit();

        for (self.datas.items, 0..) |data, index| {
            std.debug.assert(index == data.index);
            if (data.payload.isRef()) {
                try ref_datas.append(data);
                continue;
            }
            if (uni_datas.getEntry(data)) |repeated| {
                try reduces.append(.{ .from = data.index, .to = repeated.key_ptr.*.index});
            }
            else {
                try uni_datas.put(data, undefined);
            }
        }
        uni_datas.clearRetainingCapacity();

        const tryReplaceRepeated = struct {
            fn foo(replace_pairs: @TypeOf(reduces), index: *u64) void {
                // note the field `from` in `replaces` is well ordered
                for (replace_pairs.items) |pair| {
                    if (pair.from == index.*) {
                        index.* = pair.to;
                        return;
                    } else if (pair.from > index.*) return;
                }
            }
        }.foo;
        for (ref_datas.items) |data| {
            switch (data.payload) {
                .pair => |*pair| {
                    tryReplaceRepeated(reduces, &pair.@"0");
                    tryReplaceRepeated(reduces, &pair.@"1");
                },
                .list => |list| {
                    for (list.indicies.items) |*index| tryReplaceRepeated(reduces, index);
                },
                else => unreachable,
            }
        }

        const index_remaps = try allocator.alloc(usize, self.datas.items.len);
        defer allocator.free(index_remaps);
        {
            var index = reduces.items.len;
            while (index > 0) {
                index -= 1;
                const trash = self.datas.swapRemove(reduces.items[index].from);
                trash.payload.deinit();
                allocator.destroy(trash);
            }
        }
        for (self.datas.items, 0..) |data, new_index| {
            index_remaps[data.index] = new_index;
            data.index = new_index;
        }
        for (ref_datas.items) |data| {
            switch (data.payload) {
                .pair => |*pair| {
                    pair.@"0" = index_remaps[pair.@"0"];
                    pair.@"1" = index_remaps[pair.@"1"];
                },
                .list => |list| {
                    for (list.indicies.items) |*index| index.* = index_remaps[index.*];
                },
                else => unreachable,
            }
        }
    }

    pub const SaveError = Allocator.Error || File.GetSeekPosError || File.WriteError;

    pub fn saveInto(self: DataFormat, file: File) SaveError!usize {
        var offsets: std.ArrayList(u64) = .init(self.datas.allocator);
        defer offsets.deinit();
        try offsets.ensureUnusedCapacity(self.datas.items.len);

        var writed: usize = 0;
        for (self.datas.items) |data| {
            offsets.appendAssumeCapacity(try file.getPos());
            writed += try data.payload.writeInto(file);
        }

        const offsets_start = try file.getPos();
        writed += try writeVarInt(file, offsets.items.len);
        for (offsets.items) |offset| writed += try writeVarInt(file, offset);

        try file.writeAll(entry_lable);
        writed += entry_lable.len;

        writed += try writeVarInt(file, offsets_start);

        return writed;
    }

    pub const LoadConfig = struct {
        /// where to start searching file entry lable
        entry_lable_search_start: ?u64 = null,
        /// the size of searching area
        entry_lable_search_size: u64 = 128,
    };

    pub const LoadError = error {
        EntryLableNotFound,
    } || Data.Payload.ReadError || Allocator.Error || File.GetSeekPosError || File.SeekError || File.ReadError;

    pub fn loadFromAlloc(file: File, allocator: Allocator, cfg: LoadConfig) LoadError!*DataFormat {
        const entry_pos: u64 = blk: {
            const file_end = try file.getEndPos();

            const search_start = cfg.entry_lable_search_start orelse
                if (cfg.entry_lable_search_size > file_end) 0 else (file_end - cfg.entry_lable_search_size)
            ;
            std.debug.assert(search_start <= file_end);
            const search_size = @min(cfg.entry_lable_search_size, file_end - search_start);
            const search_buff = try allocator.alloc(u8, search_size);
            defer allocator.free(search_buff);

            try file.seekTo(search_start);
            const read_len = try file.readAll(search_buff);
            std.debug.assert(read_len == search_size);

            break :blk std.mem.lastIndexOf(u8, search_buff, entry_lable) orelse return LoadError.EntryLableNotFound;
        };

        try file.seekTo(entry_pos + entry_lable.len);
        const offsets_start = try readVarInt(file, u64);
        try file.seekTo(offsets_start);
        const offset_count = try readVarInt(file, u64);
        const offsets = try allocator.alloc(u64, offset_count);
        defer allocator.free(offsets);
        for (offsets) |*offset| offset.* = try readVarInt(file, u64);

        const df = try allocator.create(DataFormat);
        errdefer allocator.destroy(df);
        df.* = .init(allocator);
        errdefer df.deinit();
        try df.datas.ensureUnusedCapacity(offsets.len);

        for (offsets, 0..) |offset, index| {
            const new_data = try allocator.create(Data);
            errdefer allocator.destroy(new_data);

            try file.seekTo(offset);
            new_data.df = df;
            new_data.index = index;
            new_data.payload = try .readFrom(file, df);

            df.datas.appendAssumeCapacity(new_data);
        }

        return df;
    }
};

pub const Data = struct {
    /// should not change this
    df: *const DataFormat,
    /// should not change this
    index: u64,
    payload: Payload,

    pub const Type = enum(u8) {
        byte,
        bytes,
        string,
        integer,
        f32,
        f64,
        pair,
        list,
    };

    pub const Payload = union(Type) {
        byte: Byte,
        bytes: Bytes,
        string: String,
        integer: Integer,
        f32: F32,
        f64: F64,
        pair: Pair,
        list: List,

        pub fn isRef(self: Data.Payload) bool {
            return self == .pair or self == .list;
        }

        fn init(@"type": Data.Type, df: *const DataFormat) Allocator.Error!Data.Payload {
            return switch (@"type") { // TODO: use @typeInfo and inline for
                .byte    => .{ .byte    = .init(df) },
                .bytes   => .{ .bytes   = .init(df) },
                .string  => .{ .string  = try .init(df) },
                .integer => .{ .integer = .init(df) },
                .f32     => .{ .f32     = .init(df) },
                .f64     => .{ .f64     = .init(df) },
                .pair    => .{ .pair    = .init(df) },
                .list    => .{ .list    = .init(df) },
            };
        }

        fn deinit(self: Data.Payload) void {
            const tag = std.meta.activeTag(self);
            inline for (std.meta.fields(Data.Payload)) |field| {
                if (@field(Data.Type, field.name) == tag) {
                    @field(self, field.name).deinit();
                    return;
                }
            } else unreachable;
        }

        pub const HashContext = struct {
            pub fn hash(_: Payload.HashContext, payload: Data.Payload) u64 {
                const tag = std.meta.activeTag(payload);
                inline for (std.meta.fields(Data.Payload)) |field| {
                    if (@field(Data.Type, field.name) == tag) {
                        const pl = @field(payload, field.name);
                        return @TypeOf(pl).HashContext.hash(undefined, pl);
                    }
                } else unreachable;
            }

            pub fn eql(_: Payload.HashContext, payload1: Data.Payload, payload2: Data.Payload) bool {
                const tag = std.meta.activeTag(payload1);
                if (tag != payload2) return false;
                inline for (std.meta.fields(Data.Payload)) |field| {
                    if (@field(Data.Type, field.name) == tag) {
                        const pl = @field(payload1, field.name);
                        return @TypeOf(pl).HashContext.eql(undefined, pl, @field(payload2, field.name));
                    }
                } else unreachable;
            }
        };

        pub fn writeInto(self: Data.Payload, file: File) File.WriteError!usize {
            const tag: u8 = @intFromEnum(self);
            try file.writeAll((&tag)[0..1]);

            return 1 + switch (self) { // TODO: use @typeInfo and inline for
                .byte    => |payload| try payload.writeInto(file),
                .bytes   => |payload| try payload.writeInto(file),
                .string  => |payload| try payload.writeInto(file),
                .integer => |payload| try payload.writeInto(file),
                .f32     => |payload| try payload.writeInto(file),
                .f64     => |payload| try payload.writeInto(file),
                .pair    => |payload| try payload.writeInto(file),
                .list    => |payload| try payload.writeInto(file),
            };
        }

        pub const ReadError = error { GetInvalidDataType } || File.ReadError || Allocator.Error;

        pub fn readFrom(file: File, df: *const DataFormat) ReadError!Data.Payload {
            var type_num: u8 = 0xFF;
            _ = try file.readAll((&type_num)[0..1]);
            const @"type" = std.enums.fromInt(Data.Type, type_num) orelse return ReadError.GetInvalidDataType;

            return switch (@"type") { // TODO: use @typeInfo and inline for
                .byte    => .{ .byte    = try .readFrom(file, df) },
                .bytes   => .{ .bytes   = try .readFrom(file, df) },
                .string  => .{ .string  = try .readFrom(file, df) },
                .integer => .{ .integer = try .readFrom(file, df) },
                .f32     => .{ .f32     = try .readFrom(file, df) },
                .f64     => .{ .f64     = try .readFrom(file, df) },
                .pair    => .{ .pair    = try .readFrom(file, df) },
                .list    => .{ .list    = try .readFrom(file, df) },
            };
        }

        pub const Byte = struct {
            /// should not change this
            df: *const DataFormat,
            byte: u8,

            fn init(df: *const DataFormat) Byte {
                return .{
                    .df = df,
                    .byte = undefined,
                };
            }

            fn deinit(self: Byte) void {
                _ = self;
            }

            pub const HashContext = struct {
                pub fn hash(_: Byte.HashContext, byte: Byte) u64 {
                    return byte.byte;
                }

                pub fn eql(_: Byte.HashContext, byte1: Byte, byte2: Byte) bool {
                    return byte1.byte == byte2.byte;
                }
            };

            pub fn writeInto(self: Byte, file: File) File.WriteError!usize {
                try file.writeAll((&self.byte)[0..1]);
                return 1;
            }

            pub fn readFrom(file: File, df: *const DataFormat) File.ReadError!Byte {
                var self: Byte = .init(df);
                errdefer self.deinit();
                _ = try file.readAll((&self.byte)[0..1]);
                return self;
            }

            pub fn set(self: *Byte, val: u8) void {
                self.byte = val;
            }

            pub fn get(self: *Byte) u8 {
                return self.byte;
            }
        };

        /// always assume bytes are little endian
        pub const Bytes = struct {
            /// should not change this
            df: *const DataFormat,
            bytes: []u8,

            fn init(df: *const DataFormat) Bytes {
                return .{
                    .df = df,
                    .bytes = &.{},
                };
            }

            fn deinit(self: Bytes) void {
                self.df.datas.allocator.free(self.bytes);
            }

            pub const HashContext = struct {
                pub fn hash(_: Bytes.HashContext, bytes: Bytes) u64 {
                    return std.hash.Wyhash.hash(0, bytes.bytes);
                }

                pub fn eql(_: Bytes.HashContext, bytes1: Bytes, bytes2: Bytes) bool {
                    return std.mem.eql(u8, bytes1.bytes, bytes2.bytes);
                }
            };

            pub fn writeInto(self: Bytes, file: File) File.WriteError!usize {
                const len_len = try writeVarInt(file, self.bytes.len);
                try file.writeAll(self.bytes);
                return len_len + self.bytes.len;
            }

            pub fn readFrom(file: File, df: *const DataFormat) (File.ReadError || Allocator.Error)!Bytes {
                const len = try readVarInt(file, u64);
                const bytes = try df.datas.allocator.alloc(u8, len);
                errdefer df.datas.allocator.free(bytes);
                _ = try file.readAll(bytes);
                return .{ .df = df, .bytes = bytes };
            }

            pub fn set(self: *Bytes, val: []const u8) Allocator.Error!void {
                const allocator = self.df.datas.allocator;
                if (allocator.resize(self.bytes, val.len)) {
                    self.bytes.len = val.len;
                    @memcpy(self.bytes, val);
                }
                else {
                    allocator.free(self.bytes);
                    self.bytes.len = 0;
                    self.bytes = try allocator.dupe(u8, val);
                }
            }

            pub fn get(self: @This()) []u8 {
                return self.bytes;
            }
        };

        pub const String = struct {
            /// should not change this
            df: *const DataFormat,
            string: [:0]u8,

            fn init(df: *const DataFormat) Allocator.Error!String {
                return .{
                    .df = df,
                    .string = try df.datas.allocator.allocSentinel(u8, 0, 0),
                };
            }

            fn deinit(self: String) void {
                self.df.datas.allocator.free(self.string);
            }

            pub const HashContext = struct {
                pub fn hash(_: String.HashContext, string: String) u64 {
                    return std.hash.Wyhash.hash(0, string.string);
                }

                pub fn eql(_: String.HashContext, string1: String, string2: String) bool {
                    return std.mem.orderZ(u8, string1.string, string2.string) == .eq;
                }
            };

            pub fn writeInto(self: String, file: File) File.WriteError!usize {
                const len_len = try writeVarInt(file, self.string.len);
                try file.writeAll(self.string);
                return len_len + self.string.len;
            }

            pub fn readFrom(file: File, df: *const DataFormat) (File.ReadError || Allocator.Error)!String {
                const len = try readVarInt(file, u64);
                const string = try df.datas.allocator.allocSentinel(u8, len, 0);
                errdefer df.datas.allocator.free(string);
                _ = try file.readAll(string);
                return .{ .df = df, .string = string };
            }

            pub fn set(self: *String, val: [:0]const u8) Allocator.Error!void {
                const allocator = self.df.datas.allocator;
                const bytes = self.string[0..self.string.len + 1];
                if (allocator.resize(bytes, val.len + 1)) {
                    bytes[val.len] = 0;
                    self.string.len = val.len;
                    @memcpy(self.string, val);
                }
                else {
                    allocator.free(self.string);
                    self.string = try allocator.dupeZ(u8, val);
                }
            }

            pub fn get(self: String) [:0]u8 {
                return self.string;
            }
        };

        pub const Integer = struct {
            /// should not change this
            df: *const DataFormat,
            /// should not change this
            data: [*]u8,
            /// should not change this
            is_negative: bool,
            /// should not change this
            abs_bits: u16,

            fn init(df: *const DataFormat) Integer {
                return .{
                    .df = df,
                    .data = empty_data,
                    .is_negative = false,
                    .abs_bits = 0,
                };
            }

            fn deinit(self: Integer) void {
                self.df.datas.allocator.free(self.data[0..self.dataLength()]);
            }

            pub const HashContext = struct {
                pub fn hash(_: Integer.HashContext, int: Integer) u64 {
                    var hasher = std.hash.Wyhash.init(@intFromBool(int.is_negative));
                    hasher.update(int.data[0..int.dataLength()]);
                    return hasher.final();
                }

                pub fn eql(_: Integer.HashContext, int1: Integer, int2: Integer) bool {
                    const data_len = int1.dataLength();
                    return int1.is_negative == int2.is_negative and int1.abs_bits == int2.abs_bits
                        and std.mem.eql(u8, int1.data[0..data_len], int2.data[0..data_len]);
                }
            };

            pub fn writeInto(self: Integer, file: File) File.WriteError!usize {
                const data_len = self.dataLength();
                const header = (@as(u15, data_len) << 1) | @intFromBool(self.is_negative);
                const header_len = try writeVarInt(file, header);
                try file.writeAll(self.data[0..data_len]);
                return @as(usize, header_len) + data_len;
            }

            pub fn readFrom(file: File, df: *const DataFormat) (File.ReadError || Allocator.Error)!Integer {
                var self: Integer = .init(df);
                errdefer self.deinit();

                const header = try readVarInt(file, u15);
                const data_len: u24 = header >> 1;
                if (data_len == 0) return self;
                const data = try df.datas.allocator.alloc(u8, data_len);
                errdefer df.datas.allocator.free(data);
                _ = try file.readAll(data);

                const empty_bits = @clz(data[data_len - 1]);
                std.debug.assert(empty_bits != 8);
                const abs_bits = data_len * 8 - empty_bits;
                std.debug.assert(abs_bits <= std.math.maxInt(u16));

                self.is_negative = header & 1 != 0;
                self.abs_bits = @truncate(abs_bits);
                self.data = data.ptr;
                return self;
            }

            /// SAFETY: `val` cannot be `-2^(n-1)` for n-bit signed integer
            pub fn set(self: *Integer, val: anytype) Allocator.Error!void {
                const info = @typeInfo(@TypeOf(val));
                if (info != .int) @compileError(@typeName(Integer) ++ ".set only accepts integers");
                const UInt = std.meta.Int(.unsigned, info.int.bits);
                // `-2^(n-1)` for n-bit signed integer may cause numerical error, see `.minFitableBits`
                if (info.int.signedness == .signed) std.debug.assert(val != std.math.minInt(@TypeOf(val)));
                const MaxSInt = std.meta.Int(.signed, std.math.maxInt(u16));

                const allocator = self.df.datas.allocator;
                var data: []u8 = self.data[0..self.dataLength()];
                if (val == 0) {
                    allocator.free(data);
                    self.data = empty_data;
                    self.abs_bits = 0;
                    return;
                }

                const neg = if (@TypeOf(val) == MaxSInt)
                    val < 0 and val != std.math.minInt(MaxSInt) // see `.minFitableBits`
                else
                    val < 0 // this can determite at comptime if val is a uint
                ;
                self.is_negative = neg;
                var uint: UInt = @bitCast(val);
                if (neg) uint = -%uint;
                const bits = info.int.bits - @clz(uint);
                if (big_endian) uint = @byteSwap(uint);
                const bytes: [*]const u8 = @ptrCast(&uint);

                const data_len: u14 = @truncate((bits / 8) + @intFromBool(bits % 8 != 0));
                if (!allocator.resize(data, data_len)) {
                    allocator.free(data);
                    self.abs_bits = 0;
                    data = try allocator.alloc(u8, data_len);
                    self.data = data.ptr;
                }
                self.abs_bits = bits;
                @memcpy(data, bytes);

                const extra_bits: u3 = @truncate(self.abs_bits % 8);
                if (extra_bits != 0) { // clean dirty data outside the integer in last byte
                    const data_mask = (@as(u8, 1) << extra_bits) - 1;
                    data[data_len - 1] &= data_mask;
                }
            }

            /// truncate if the bit size of `Int` is less than `.minFitableBits()`,
            /// always use bit cast regardless of the signedness.
            pub fn get(self: Integer, comptime Int: type) Int {
                const info = @typeInfo(Int);
                if (info != .int) @compileError(@typeName(Integer) ++ ".get only accepts integer types");
                const IntBuffer = [@sizeOf(Int)] u8;

                if (self.abs_bits == 0) return 0;

                var int_buffer = std.mem.zeroes(IntBuffer);
                @memcpy(int_buffer[0..@min(int_buffer.len, self.dataLength())], self.data);

                const int: *Int = @ptrCast(@alignCast(&int_buffer));
                if (big_endian) int.* = @byteSwap(int.*);
                if (self.is_negative) int.* = -%int.*;

                return int.*;
            }

            pub fn minFitableBits(self: Integer) u16 {
                if (self.abs_bits == 0) return 0;
                // ? check for `-2^n`, `-2^n` can fit into n-bit instead of (n+1)-bit integer.
                return std.math.add(u16, self.abs_bits, @intFromBool(self.is_negative)) catch unreachable;
            }

            const empty_data: [*]u8 = @ptrFromInt(std.math.maxInt(usize));

            fn dataLength(self: Integer) u14 {
                return @truncate((self.abs_bits / 8) + @intFromBool(self.abs_bits % 8 != 0));
            }
        };

        pub const F32 = struct {
            /// should not change this
            df: *const DataFormat,
            le_f32: f32,

            fn init(df: *const DataFormat) F32 {
                return .{
                    .df = df,
                    .le_f32 = undefined,
                };
            }

            fn deinit(self: F32) void {
                _ = self;
            }

            pub const HashContext = struct {
                pub fn hash(_: F32.HashContext, f: F32) u64 {
                    return std.hash.int(@as(u32, @bitCast(f.le_f32)));
                }

                pub fn eql(_: F32.HashContext, f1: F32, f2: F32) bool {
                    return @as(u32, @bitCast(f1.le_f32)) == @as(u32, @bitCast(f2.le_f32));
                }
            };

            pub fn writeInto(self: F32, file: File) File.WriteError!usize {
                const bit: [@sizeOf(f32)]u8 = @bitCast(self.le_f32);
                try file.writeAll(&bit);
                return @sizeOf(f32);
            }

            pub fn readFrom(file: File, df: *const DataFormat) File.ReadError!F32 {
                var self: F32 = .init(df);
                errdefer self.deinit();
                const ptr: [*]u8 = @ptrCast(&self.le_f32);
                _ = try file.readAll(ptr[0..@sizeOf(f32)]);
                return self;
            }

            pub fn set(self: *F32, val: f32) void {
                if (big_endian) {
                    const i: u32 = @bitCast(val);
                    const le_i = @byteSwap(i);
                    self.le_f32 = @bitCast(le_i);
                }
                else {
                    self.le_f32 = val;
                }
            }

            pub fn get(self: *F32) f32 {
                if (big_endian) {
                    const le_i: u32 = @bitCast(self.le_f32);
                    const i = @byteSwap(le_i);
                    return @bitCast(i);
                }
                else {
                    return self.le_f32;
                }
            }
        };

        pub const F64 = struct {
            /// should not change this
            df: *const DataFormat,
            le_f64: f64,

            fn init(df: *const DataFormat) F64 {
                return .{
                    .df = df,
                    .le_f64 = undefined,
                };
            }

            fn deinit(self: F64) void {
                _ = self;
            }

            pub const HashContext = struct {
                pub fn hash(_: F64.HashContext, f: F64) u64 {
                    return std.hash.int(@as(u64, @bitCast(f.le_f64)));
                }

                pub fn eql(_: F64.HashContext, f1: F64, f2: F64) bool {
                    return @as(u64, @bitCast(f1.le_f64)) == @as(u64, @bitCast(f2.le_f64));
                }
            };

            pub fn writeInto(self: F64, file: File) File.WriteError!usize {
                const bit: [@sizeOf(f64)]u8 = @bitCast(self.le_f64);
                try file.writeAll(&bit);
                return @sizeOf(f64);
            }

            pub fn readFrom(file: File, df: *const DataFormat) File.ReadError!F64 {
                var self: F64 = .init(df);
                errdefer self.deinit();
                const ptr: [*]u8 = @ptrCast(&self.le_f64);
                _ = try file.readAll(ptr[0..@sizeOf(f64)]);
                return self;
            }

            pub fn set(self: *F64, val: f64) void {
                if (big_endian) {
                    const i: u64 = @bitCast(val);
                    const le_i = @byteSwap(i);
                    self.le_f64 = @bitCast(le_i);
                }
                else {
                    self.le_f64 = val;
                }
            }

            pub fn get(self: *F64) f64 {
                if (big_endian) {
                    const le_i: u64 = @bitCast(self.le_f64);
                    const i = @byteSwap(le_i);
                    return @bitCast(i);
                }
                else {
                    return self.le_f64;
                }
            }
        };

        pub const Pair = struct {
            /// should not change this
            df: *const DataFormat,
            @"0": u64,
            @"1": u64,

            fn init(df: *const DataFormat) Pair {
                return .{
                    .df = df,
                    .@"0" = undefined,
                    .@"1" = undefined,
                };
            }

            fn deinit(self: Pair) void {
                _ = self;
            }

            pub const HashContext = struct {
                pub fn hash(_: Pair.HashContext, pair: Pair) u64 {
                    var hasher = std.hash.Wyhash.init(0);
                    hasher.update(std.mem.asBytes(&pair.@"0"));
                    hasher.update(std.mem.asBytes(&pair.@"1"));
                    return hasher.final();
                }

                pub fn eql(_: Pair.HashContext, pair1: Pair, pair2: Pair) bool {
                    return pair1.@"0" == pair2.@"0" and pair1.@"1" == pair2.@"1";
                }
            };

            pub fn writeInto(self: Pair, file: File) File.WriteError!usize {
                return try writeVarInt(file, self.@"0") + try writeVarInt(file, self.@"1");
            }

            pub fn readFrom(file: File, df: *const DataFormat) File.ReadError!Pair {
                var self: Pair = .init(df);
                errdefer self.deinit();
                self.@"0" = try readVarInt(file, u64);
                self.@"1" = try readVarInt(file, u64);
                return self;
            }

            pub const SetError = error { NotInTheSameDataFormat };

            pub fn set0(self: *Pair, data: *const Data) SetError!void {
                if (self.df != data.df) return SetError.NotInTheSameDataFormat;
                self.@"0" = data.index;
            }
            pub fn set1(self: *Pair, data: *const Data) SetError!void {
                if (self.df != data.df) return SetError.NotInTheSameDataFormat;
                self.@"1" = data.index;
            }
            pub fn set(self: *Pair, @"0": *const Data, @"1": *const Data) SetError!void {
                try self.set0(@"0");
                try self.set1(@"1");
            }

            pub fn get0(self: Pair) ?*Data {
                const datas = self.df.datas;
                if (self.@"0" >= datas.items.len) return null;
                return datas.items[self.@"0"];
            }
            pub fn get1(self: Pair) ?*Data {
                const datas = self.df.datas;
                if (self.@"1" >= datas.items.len) return null;
                return datas.items[self.@"1"];
            }
        };

        pub const List = struct {
            /// should not change this
            df: *const DataFormat,
            indicies: std.ArrayList(u64),

            fn init(df: *const DataFormat) List {
                return .{
                    .df = df,
                    .indicies = .init(df.datas.allocator),
                };
            }

            fn deinit(self: List) void {
                self.indicies.deinit();
            }

            pub const HashContext = struct {
                pub fn hash(_: List.HashContext, list: List) u64 {
                    var hasher = std.hash.Wyhash.init(0);
                    for (list.indicies.items) |index| hasher.update(std.mem.asBytes(&index));
                    return hasher.final();
                }

                pub fn eql(_: List.HashContext, list1: List, list2: List) bool {
                    return std.mem.eql(u64, list1.indicies.items, list2.indicies.items);
                }
            };

            pub fn writeInto(self: List, file: File) File.WriteError!usize {
                var count = try writeVarInt(file, self.indicies.items.len);
                for (self.indicies.items) |index| count += try writeVarInt(file, index);
                return count;
            }

            pub fn readFrom(file: File, df: *const DataFormat) (File.ReadError || Allocator.Error)!List {
                var self: List = .init(df);
                errdefer self.deinit();

                var count = try readVarInt(file, u64);
                try self.indicies.ensureUnusedCapacity(count);
                while (count > 0) : (count -= 1) {
                    self.indicies.appendAssumeCapacity(try readVarInt(file, u64));
                }

                return self;
            }

            pub const AppendError = error { NotInTheSameDataFormat } || Allocator.Error;

            pub fn append(self: *List, data: *const Data) AppendError!void {
                if (self.df != data.df) return AppendError.NotInTheSameDataFormat;
                try self.indicies.append(data.index);
            }

            pub fn appendSlise(self: *List, datas: []const *const Data) AppendError!void {
                for (datas) |data| {
                    if (self.df != data.df) return AppendError.NotInTheSameDataFormat;
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

            pub fn pop(self: *List) ?*Data {
                const datas = self.df.datas;
                const index = self.indicies.pop() orelse return null;
                if (index >= datas.items.len) return null;
                return datas.items[index];
            }

            pub fn get(self: List, index: usize) ?*Data {
                const datas = self.df.datas;
                if (index >= self.indicies.items.len) return null;
                const k = self.indicies.items[index];
                if (k >= datas.items.len) return null;
                return datas.items[k];
            }
        };
    };
};


fn writeVarInt(file: File, int: anytype) File.WriteError!usize {
    const info = @typeInfo(@TypeOf(int));
    if (info != .int and info.int.signedness == .unsigned) @compileError("only accepts unsigned integer");

    const first: u8 = @truncate(int & 0x7F);
    try file.writeAll((&first)[0..1]);

    var count: usize = 1;
    var i = int >> 7;
    while (i > 0) : (i >>= 7) {
        const byte: u8 = 0x80 | @as(u8, @truncate(int & 0x7F));
        try file.writeAll((&byte)[0..1]);
        count += 1;
    }
    return count;
}

fn readVarInt(file: File, comptime Int: type) File.ReadError!Int {
    const info = @typeInfo(Int);
    if (info != .int and info.int.signedness == .unsigned) @compileError("only accepts unsigned integer");

    var byte: u8 = undefined;
    _ = try file.readAll((&byte)[0..1]);
    var int: Int = byte & 0x7F;
    while (byte & 0x80 != 0) : (int = (int << 7) & (byte & 0x7F)) {
        _ = try file.readAll((&byte)[0..1]);
    }
    return int;
}
