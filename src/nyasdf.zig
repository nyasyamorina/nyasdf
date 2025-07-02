const builtin = @import("builtin");
const std = @import("std");

const Allocator = std.mem.Allocator;


pub const Content = struct {
    datas: std.ArrayList(*Data),

    pub fn init(allocator: Allocator) Content {
        return .{ .datas = .init(allocator) };
    }

    pub fn deinit(self: Content) void {
        const allocator = self.datas.allocator;

        for (self.datas.items) |data| {
            switch (data.payload) { // TODO: use @typeInfo and inline for
                .byte    => |payload| payload.deinit(),
                .bytes   => |payload| payload.deinit(),
                .string  => |payload| payload.deinit(),
                .integer => |payload| payload.deinit(),
                .f32     => |payload| payload.deinit(),
                .f64     => |payload| payload.deinit(),
                .pair    => |payload| payload.deinit(),
                .list    => |payload| payload.deinit(),
            }
            allocator.destroy(data);
        }
        self.datas.deinit();
    }

    pub fn newData(self: *Content, @"type": Data.Type) Allocator.Error!*Data {
        const allocator = self.datas.allocator;

        const new_data = try allocator.create(Data);
        errdefer allocator.destroy(new_data);
        try self.datas.append(new_data);
        errdefer _ = self.datas.pop();

        new_data.* = .{
            .df_ctn = self,
            .index = self.datas.items.len - 1,
            .payload = undefined,
        };
        new_data.payload = switch (@"type") { // TODO: use @typeInfo and inline for
            .byte =>    .{ .byte    = .init(self) },
            .bytes =>   .{ .bytes   = .init(self) },
            .string =>  .{ .string  = .init(self) },
            .integer => .{ .integer = .init(self) },
            .f32 =>     .{ .f32     = .init(self) },
            .f64 =>     .{ .f64     = .init(self) },
            .pair =>    .{ .pair    = .init(self) },
            .list =>    .{ .list    = .init(self) },
        };
        return new_data;
    }
};

pub const Data = struct {
    df_ctn: *const Content,
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

        const types = @import("types.zig");
        pub const Byte = types.Byte;
        pub const Bytes = types.Bytes;
        pub const String = types.String;
        pub const Integer = types.Integer;
        pub const F32 = types.F32;
        pub const F64 = types.F64;
        pub const Pair = types.Pair;
        pub const List = types.List;
    };
};
