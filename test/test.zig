const std = @import("std");

const nyasdf = @import("nyasdf");


test "save int" {
    const file = try std.fs.cwd().createFile("test.save int.nyasdf", .{
        .read = true,
        .truncate = true,
    });
    defer file.close();

    const mgr: nyasdf.Manager = .init(file);
    defer mgr.deinit();

    const e0 = mgr.entry(0);
    const s0 = try e0.saveInt(@as(u64, 0xFFF));
    try std.testing.expectEqual(4, s0);

    const e1 = mgr.entry(e0.offset + s0);
    const IntMax = std.meta.Int(.signed, 8 * 0x7F);
    const s1 = try e1.saveInt(@as(IntMax, -1));
    try std.testing.expectEqual(3, s1);

    const e2 = mgr.entry(e1.offset + s1);
    const IntTooLong = std.meta.Int(.signed, 8 * 0x7F + 2);
    const es2 = e2.saveInt(@as(IntTooLong, std.math.minInt(IntTooLong) + 1));
    try std.testing.expectError(nyasdf.Entry.SaveIntError.IntTooBig, es2);

    const s2 = try e2.saveInt(@as(i32, 0));
    try std.testing.expectEqual(2, s2);
}

test "load int" {
    const file = try std.fs.cwd().createFile("test.load int.nyasdf", .{
        .read = true,
        .truncate = true,
    });
    defer file.close();

    const mgr: nyasdf.Manager = .init(file);
    defer mgr.deinit();

    const n0: u64 = 0xFFF;
    const e0 = mgr.entry(0);
    const s0 = try e0.saveInt(n0);
    try std.testing.expectEqual(n0, try e0.loadInt(u64, false));

    const n1: i31 = -1;
    const e1 = mgr.entry(e0.offset + s0);
    const s1 = try e1.saveInt(n1);
    try std.testing.expectEqual(n1, try e1.loadInt(i32, false));
    try std.testing.expectError(nyasdf.Entry.LoadIntError.NegativeNeedSignedInteger, e1.loadInt(u31, false));

    const n2: u69 = std.math.maxInt(u69);
    const e2 = mgr.entry(e1.offset + s1);
    const s2 = try e2.saveInt(n2);
    try std.testing.expectError(nyasdf.Entry.LoadIntError.IntTooBig, e2.loadInt(u68, false));
    try std.testing.expectEqual(@as(u68, @truncate(n2)), try e2.loadInt(u68, true));

    const n3: u7 = 0;
    const e3 = mgr.entry(e2.offset + s2);
    _ = try e3.saveInt(n3);
    try std.testing.expectEqual(n3, try e3.loadInt(u7, false));
}
