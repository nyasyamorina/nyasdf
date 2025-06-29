const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const nyasdf = b.addStaticLibrary(.{
        .name = "nyasdf",
        .root_source_file = b.path("src/nyasdf.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(nyasdf);


    const test_module = b.createModule(.{
        .root_source_file = b.path("test/test.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_module.addImport("nyasdf", nyasdf.root_module);

    const mod_tests = b.addTest(.{ .root_module = test_module });
    const run_mod_tests = b.addRunArtifact(mod_tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_mod_tests.step);

    const test_filters = b.option([]const []const u8, "test-filter", "Skip tests that do not match any filter") orelse &.{};
    const mod_test_unit = b.addTest(.{
        .root_module = test_module,
        .filters = test_filters,
    });
    const run_mod_test_unit = b.addRunArtifact(mod_test_unit);
    const test_unit_step = b.step("test-filter", "Run test with filter");
    test_unit_step.dependOn(&run_mod_test_unit.step);
}
