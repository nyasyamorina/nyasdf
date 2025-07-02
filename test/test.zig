const std = @import("std");

const nyasdf = @import("nyasdf");


test "nyasdf.Content" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    var ctn: nyasdf.Content = .init(gpa.allocator());
    defer ctn.deinit();

    const v1: u8 = 0xFF;
    var d1 = try ctn.newData(.byte);
    d1.payload.byte.set(v1);
    try std.testing.expectEqual(v1, d1.payload.byte.get());

    const v2 = [_]u8 { 2, 5, 8, 11, 55, 99, 127, 250 };
    var d2 = try ctn.newData(.bytes);
    try d2.payload.bytes.set(&v2);
    try std.testing.expectEqualSlices(u8, &v2, d2.payload.bytes.get());

    const v3 = "完全变成jb的形状了（";
    var d3 = try ctn.newData(.string);
    try d3.payload.string.set(v3);
    try std.testing.expectEqualStrings(v3, d3.payload.string.get());

    const v4: i32 = -1;
    var d4 = try ctn.newData(.integer);
    try d4.payload.integer.set(v4);
    try std.testing.expectEqual(std.math.maxInt(u64), d4.payload.integer.get(u64));
    try std.testing.expectEqual(-1, d4.payload.integer.get(i16));

    const v4_: i65535 = std.math.maxInt(i65535);
    try d4.payload.integer.set(v4_);
    try std.testing.expectEqual(65534, d4.payload.integer.minFitableBits());

    const v5: f32 = 123.456;
    var d5 = try ctn.newData(.f32);
    d5.payload.f32.set(v5);
    try std.testing.expectEqual(v5, d5.payload.f32.get());

    const v6: f32 = 12345.67890;
    var d6 = try ctn.newData(.f64);
    d6.payload.f64.set(v6);
    try std.testing.expectEqual(v6, d6.payload.f64.get());

    var d7 = try ctn.newData(.pair);
    try d7.payload.pair.set(d3, d6);
    try std.testing.expectEqual(d3, d7.payload.pair.get0());
    try std.testing.expectEqual(d6, d7.payload.pair.get1());

    var d8 = try ctn.newData(.list);
    const list = &d8.payload.list;
    try list.append(d7);
    try list.appendSlise(&.{ d6, d5, d4 });
    try std.testing.expectEqual(4, list.length());
    try std.testing.expectEqual(d4, list.pop());
    try std.testing.expectEqual(3, list.length());
    try std.testing.expectEqual(d7, list.get(0));
    try std.testing.expectEqual(d5, list.get(2));
    try std.testing.expectEqual(null, list.get(3));
}
