const std = @import("std");
const Allocator = std.mem.Allocator;

const nyasdf = @import("nyasdf");
const nyasData = nyasdf.Data;
const DataList = std.ArrayList(*nyasData);
const Reducer = nyasdf.Reducer;
const WriteIterator = nyasdf.WriteIterator;

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;
const expectEqualStrings = std.testing.expectEqualStrings;


test "nyasdf.Data" {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();

    const d1: nyasData = .{ .bool = .{ .val = true } };
    try expectEqual(true, d1.bool.val);

    const d2: nyasData = .{ .bytes = .{ .val = &.{ 1, 2, 3, 4 } } };
    try expectEqualSlices(u8, &.{ 1, 2, 3, 4 }, d2.bytes.val);

    const d3: nyasData = .{ .f16 = .{ .val = 16.0 } };
    try expectEqual(16.0, d3.f16.val);

    const d4: nyasData = .{ .f32 = .{ .val = 32.0 } };
    try expectEqual(32.0, d4.f32.val);

    const d5: nyasData = .{ .f64 = .{ .val = 64.0 } };
    try expectEqual(64.0, d5.f64.val);

    var d6: nyasData = .{ .int = .init(gpa.allocator()) };
    defer d6.deinit();
    try d6.int.set(@as(i42, -42));
    try expectEqual(@as(i42, -42), d6.int.get(i42));
    try expectEqual(@as(u64, @bitCast(@as(i64, -42))), d6.int.get(u64));
    try expectEqual(@as(i6, @truncate(@as(i42, -42))), d6.int.get(i6));
    try expectEqual(@as(u6, @bitCast(@as(i6, @truncate(@as(i42, -42))))), d6.int.get(u6));
    try d6.int.set(-(@as(i64, 1) << 41));
    try expectEqual(42, d6.int.minFitableBits());
    try d6.int.set(@as(i65535, std.math.minInt(i65535)));
    try expectEqual(65535, d6.int.minFitableBits());

    var d7_val = [_]*const nyasData {&d1, &d2, &d3};
    var d7: nyasData = .{ .list = .{ .val = .fromOwnedSlice(&d7_val) } };
    try expectEqualSlices(*const nyasData, &.{&d1, &d2, &d3}, d7.list.val.items);

    const d8: nyasData = .{ .null = .{} };

    const d9: nyasData = .{ .pair = .{ .@"0" = &d7, .@"1" = &d8 } };
    try expectEqual(&d7, d9.pair.@"0");
    try expectEqual(&d8, d9.pair.@"1");

    const d10: nyasData = .{ .string = .{ .val = "a1b2c3" } };
    try expectEqualStrings("a1b2c3", d10.string.val);
}

test "nyasdf.Reducer" {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();

    var d0: nyasData = .{ .byte = .{ .val = 0 } };
    var d1: nyasData = .{ .pair = undefined };
    var d2: nyasData = .{ .pair = undefined };
    var d3: nyasData = .{ .pair = undefined };
    var d4: nyasData = .{ .pair = undefined };
    var d5: nyasData = .{ .list = undefined };
    var d6: nyasData = .{ .byte = .{ .val = 0 } };

    d1.pair = .{ .@"0" = &d0, .@"1" = &d0 };
    d2.pair = .{ .@"0" = &d6, .@"1" = &d0 };
    d3.pair = .{ .@"0" = &d5, .@"1" = &d2 };
    d4.pair = .{ .@"0" = &d5, .@"1" = &d1 };
    var d5_val = [_]*const nyasData {&d0, &d6, &d1, &d2, &d3, &d4};
    d5.list = .{ .val = .fromOwnedSlice(&d5_val) };

    var df = [_]*nyasData { &d0, &d1, &d2, &d3, &d4, &d5, &d6 };

    var reducer: Reducer = .init(gpa.allocator());
    defer reducer.deinit();
    if (try reducer.reduceStart(&df)) {
        while (try reducer.reduceContinue()) {}
    }
    const reduced_df = reducer.keepingSlice();

    try expectEqual(&df, reduced_df.ptr);
    try expectEqual(4, reduced_df.len);
    try expect(std.mem.indexOfScalar(*nyasData, reduced_df, &d0) != null);
    try expect(std.mem.indexOfScalar(*nyasData, reduced_df, &d1) != null);
    try expect(std.mem.indexOfScalar(*nyasData, reduced_df, &d2) == null);
    try expect(std.mem.indexOfScalar(*nyasData, reduced_df, &d3) != null);
    try expect(std.mem.indexOfScalar(*nyasData, reduced_df, &d4) == null);
    try expect(std.mem.indexOfScalar(*nyasData, reduced_df, &d5) != null);
    try expect(std.mem.indexOfScalar(*nyasData, reduced_df, &d6) == null);

    try expectEqual(nyasData.Pair { .@"0" = &d0, .@"1" = &d0 }, d1.pair);
    try expectEqual(nyasData.Pair { .@"0" = &d5, .@"1" = &d1 }, d3.pair);
    try expectEqualSlices(*const nyasData, &.{ &d0, &d0, &d1, &d1, &d3, &d3 }, d5.list.val.items);
}

fn testSave(allocator: Allocator, file: std.fs.File) !usize {
    const d0: nyasData = .{ .byte = .{ .val = 0xFF } };
    const d1: nyasData = .{ .bytes = .{ .val = &.{1, 2, 3, 4} } };
    const d2: nyasData = .{ .string = .{ .val = "The quick brown fox jumps over the lazy dog" } };
    var d3: nyasData = .{ .int = .init(allocator) };
    defer d3.deinit();
    try d3.int.set(@as(i42, 0x123456789));
    var d4: nyasData = .{ .int = .init(allocator) };
    defer d4.deinit();
    try d4.int.set(@as(i42, -0x123456789));
    const d5: nyasData = .{ .f32 = .{ .val = 42.0 } };
    const d6: nyasData = .{ .f64 = .{ .val = 42.0 } };
    const d7: nyasData = .{ .pair = .{ .@"0" = &d2, .@"1" = &d4 } };
    var d8_val = [_]*const nyasData {&d0, &d3, &d4, &d5, &d6};
    const d8: nyasData = .{ .list = .{ .val = .fromOwnedSlice(&d8_val) }};

    return try nyasdf.FileWriteIterator.writeDirect(allocator, file, &.{&d0, &d1, &d2, &d3, &d4, &d5, &d6, &d7, &d8});
}
test "nyasdf.WriteIterator" {
    const file = try std.fs.cwd().createFile("test.write.nyasdf", .{});
    defer file.close();

    const len = try testSave(std.testing.allocator, file);
    try expectEqual(108, len);
}

test "nyasdf.ReadIterator" {
    {
        const file = try std.fs.cwd().createFile("test.read.nyasdf", .{});
        defer file.close();
        _ = try testSave(std.testing.allocator, file);
    }

    const file = try std.fs.cwd().openFile("test.read.nyasdf", .{});
    defer file.close();

    var data_list = try nyasdf.FileReadIterator.readDirectAlloc(std.testing.allocator, file);
    defer {
        nyasdf.destroyAllData(std.testing.allocator, data_list.items);
        data_list.deinit(std.testing.allocator);
    }

    try expectEqual(0xFF, data_list.items[0].byte.val);
    try expectEqualSlices(u8, &.{1, 2, 3, 4}, data_list.items[1].bytes.val);
    try expectEqualStrings("The quick brown fox jumps over the lazy dog", data_list.items[2].string.val);
    try expectEqual(0x123456789, data_list.items[3].int.get(i42));
    try expectEqual(-0x123456789, data_list.items[4].int.get(i42));
    try expectEqual(@as(f32, 42.0), data_list.items[5].f32.val);
    try expectEqual(@as(f64, 42.0), data_list.items[6].f64.val);
    try expectEqual(data_list.items[2], data_list.items[7].pair.@"0");
    try expectEqual(data_list.items[4], data_list.items[7].pair.@"1");
    const ref_to_idx = [_]usize {0, 3, 4, 5, 6};
    var ref_to: [ref_to_idx.len]*const nyasData = undefined;
    for (&ref_to, ref_to_idx) |*to, idx| to.* = data_list.items[idx];
    try expectEqualSlices(*const nyasData, &ref_to, data_list.items[8].list.val.items);
}
