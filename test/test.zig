const std = @import("std");

const nyasdf = @import("nyasdf");


test "nyasdf.Content" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    var ctn: nyasdf.DataFormat = .init(gpa.allocator());
    defer ctn.deinit();

    const v1: u8 = 0xFF;
    var d1 = try ctn.new(.byte);
    d1.payload.byte.set(v1);
    try std.testing.expectEqual(v1, d1.payload.byte.get());

    const v2 = [_]u8 { 2, 5, 8, 11, 55, 99, 127, 250 };
    var d2 = try ctn.new(.bytes);
    try d2.payload.bytes.set(&v2);
    try std.testing.expectEqualSlices(u8, &v2, d2.payload.bytes.get());

    const v3 = "完全变成jb的形状了（";
    var d3 = try ctn.new(.string);
    try d3.payload.string.set(v3);
    try std.testing.expectEqualStrings(v3, d3.payload.string.get());

    const v4: i32 = -1;
    var d4 = try ctn.new(.integer);
    try d4.payload.integer.set(v4);
    try std.testing.expectEqual(std.math.maxInt(u64), d4.payload.integer.get(u64));
    try std.testing.expectEqual(-1, d4.payload.integer.get(i16));

    const v4_: i65535 = std.math.minInt(i65535) + 1;
    try d4.payload.integer.set(v4_);
    try std.testing.expectEqual(65535, d4.payload.integer.minFitableBits());

    const v5: f32 = 123.456;
    var d5 = try ctn.new(.f32);
    d5.payload.f32.set(v5);
    try std.testing.expectEqual(v5, d5.payload.f32.get());

    const v6: f32 = 12345.67890;
    var d6 = try ctn.new(.f64);
    d6.payload.f64.set(v6);
    try std.testing.expectEqual(v6, d6.payload.f64.get());

    var d7 = try ctn.new(.pair);
    try d7.payload.pair.set(d3, d6);
    try std.testing.expectEqual(d3, d7.payload.pair.tryGet0());
    try std.testing.expectEqual(d6, d7.payload.pair.tryGet1());

    const list = try ctn.newList();
    try list.append(d7);
    try list.appendSlise(&.{ d6, d5, d4 });
    try std.testing.expectEqual(4, list.length());
    try std.testing.expectEqual(d4, list.pop());
    try std.testing.expectEqual(3, list.length());
    try std.testing.expectEqual(d7, list.tryGet(0));
    try std.testing.expectEqual(d5, list.tryGet(2));
    try std.testing.expectEqual(null, list.tryGet(3));
}

test "nyasdf.Data.Type.isRef" {
    try std.testing.expect(!nyasdf.Data.Type.byte.isRef());
    try std.testing.expect(!nyasdf.Data.Type.bytes.isRef());
    try std.testing.expect(!nyasdf.Data.Type.string.isRef());
    try std.testing.expect(!nyasdf.Data.Type.integer.isRef());
    try std.testing.expect(!nyasdf.Data.Type.f32.isRef());
    try std.testing.expect(!nyasdf.Data.Type.f64.isRef());
    try std.testing.expect(nyasdf.Data.Type.pair.isRef());
    try std.testing.expect(nyasdf.Data.Type.list.isRef());
}

fn testSave(df: *nyasdf.DataFormat, file: std.fs.File) !usize {
    const d0 = try df.new(.byte);
    d0.payload.byte.set(0xFF);

    try (try df.newBytes()).set(&.{ 1, 2, 3, 4 });

    const string = try df.newString();
    try string.set("The quick brown fox jumps over the lazy dog");
    const payload: *nyasdf.Data.Payload = @fieldParentPtr("string", string);
    const d2: *nyasdf.Data = @fieldParentPtr("payload", payload);

    const d3 = try df.new(.integer);
    try d3.payload.integer.set(@as(i34, 0x123456789));

    const d4 = try df.new(.integer);
    try d4.payload.integer.set(@as(i34, -0x123456789));

    const d5 = try df.new(.f32);
    d5.payload.f32.set(std.math.nan(f32));

    const d6 = try df.new(.f64);
    d6.payload.f64.set(std.math.inf(f64));

    const d7 = try df.new(.pair);
    try d7.payload.pair.set(d2, d4);

    const list = &(try df.new(.list)).payload.list;
    try list.append(d7);
    try list.appendSlise(&.{ d0, d3, d4, d5, d6 });

    return df.saveInto(file);
}

test "save" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const file = try std.fs.cwd().createFile("test.save.nyasdf", .{});
    defer file.close();

    var df: nyasdf.DataFormat = .init(gpa.allocator());
    defer df.deinit();

    try std.testing.expectEqual(109, try testSave(&df, file));
}

test "load" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const file = try std.fs.cwd().createFile("test.load.nyasdf", .{ .read = true });
    defer file.close();

    var df1: nyasdf.DataFormat = .init(gpa.allocator());
    defer df1.deinit();
    _ = try testSave(&df1, file);

    const df2: *const nyasdf.DataFormat = try .loadFromAlloc(file, gpa.allocator(), .{});
    defer { df2.deinit(); gpa.allocator().destroy(df2); }

    try std.testing.expectEqual(9, df2.length());

    const d0 = df2.getData(0);
    try std.testing.expectEqual(0xFF, d0.?.payload.byte.get());

    try std.testing.expectEqualSlices(u8, &.{ 1, 2, 3, 4 }, df2.getData(1).?.payload.bytes.get());

    const d2 = df2.getData(2).?;
    try std.testing.expectEqualStrings("The quick brown fox jumps over the lazy dog", d2.payload.string.get());

    const d3 = df2.getData(3).?;
    try std.testing.expectEqual(@as(i34, 0x123456789), d3.payload.integer.get(i34));

    const d4 = df2.getData(4).?;
    try std.testing.expectEqual(@as(i34, -0x123456789), d4.payload.integer.get(i34));

    const d5 = df2.getData(5).?;
    try std.testing.expectEqual(@as(u32, @bitCast(std.math.nan(f32))), (@as(u32, @bitCast(d5.payload.f32.get()))));

    const d6 = df2.getData(6).?;
    try std.testing.expectEqual(@as(u64, @bitCast(std.math.inf(f64))), (@as(u64, @bitCast(d6.payload.f64.get()))));

    const pair = df2.getData(7).?.payload.pair;
    try std.testing.expectEqual(d2, pair.tryGet0());
    try std.testing.expectEqual(d4, pair.tryGet1());

    const list = df2.datas.items[8].payload.list;
    try std.testing.expectEqual(df2.getData(7), list.tryGet(0));
    try std.testing.expectEqual(d0, list.tryGet(1));
    try std.testing.expectEqual(d3, list.tryGet(2));
    try std.testing.expectEqual(d4, list.tryGet(3));
    try std.testing.expectEqual(d5, list.tryGet(4));
    try std.testing.expectEqual(d6, list.tryGet(5));
}

test "reduce value data" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    var df: nyasdf.DataFormat = .init(gpa.allocator());
    defer df.deinit();

    const test1 = 0xFF;
    const test2 = "a1b2c3";

    const d0 = try df.new(.byte);
    d0.payload.byte.set(test1);

    const d1 = try df.new(.list);
    try d1.payload.list.append(d0);

    const d2 = try df.new(.byte);
    d2.payload.byte.set(test1);

    const d3 = try df.new(.string);
    try d3.payload.string.set(test2);
    try d1.payload.list.appendSlise(&.{ d3, d2 });

    const d4 = try df.new(.pair);
    try d1.payload.list.append(d4);

    const d5 = try df.new(.string);
    try d5.payload.string.set(test2);
    try d4.payload.pair.set(d5, d0);

    try std.testing.expectEqual(6, df.length());
    try df.reduce();
    try std.testing.expectEqual(4, df.length());

    try std.testing.expect(df.contain(d0));
    try std.testing.expect(df.contain(d1));
    try std.testing.expect(!df.contain(d2));
    try std.testing.expect(df.contain(d3));
    try std.testing.expect(df.contain(d4));
    try std.testing.expect(!df.contain(d5));

    try std.testing.expectEqual(d3, d4.payload.pair.tryGet0());
    try std.testing.expectEqual(d0, d4.payload.pair.tryGet1());

    const expect_list = [_]*nyasdf.Data { d0, d3, d0, d4 };
    try std.testing.expectEqual(expect_list.len, d1.payload.list.length());
    var iter = d1.payload.list.iter();
    for (expect_list) |data| {
        try std.testing.expectEqual(data, iter.next());
    }
}

test "reduce ref data" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    var df: nyasdf.DataFormat = .init(gpa.allocator());
    defer df.deinit();

    const d0 = try df.new(.byte);
    d0.payload.byte.set(0);

    const d1 = try df.new(.pair);
    const d2 = try df.new(.pair);
    const d3 = try df.new(.pair);
    const d4 = try df.new(.pair);
    const d5 = try df.new(.list);

    const d6 = try df.new(.byte);
    d6.payload.byte.set(0);

    try d1.payload.pair.set(d0, d0);
    try d2.payload.pair.set(d6, d0);
    try d3.payload.pair.set(d5, d2);
    try d4.payload.pair.set(d5, d1);
    try d5.payload.list.appendSlise(&.{d0, d6, d1, d2, d3, d4});

    try std.testing.expectEqual(7, df.length());
    try df.reduce();
    try std.testing.expectEqual(4, df.length());

    try std.testing.expect(df.contain(d0));
    try std.testing.expect(df.contain(d1));
    try std.testing.expect(!df.contain(d2));
    try std.testing.expect(df.contain(d3));
    try std.testing.expect(!df.contain(d4));
    try std.testing.expect(df.contain(d5));
    try std.testing.expect(!df.contain(d6));

    try std.testing.expectEqual(.{d0, d0}, d1.payload.pair.get());
    try std.testing.expectEqual(.{d5, d1}, d3.payload.pair.get());
    const expect_list = [_]*const nyasdf.Data {d0, d0, d1, d1, d3, d3};
    var iter = d5.payload.list.iter();
    for (expect_list) |data| {
        try std.testing.expectEqual(data, iter.next());
    }
}
