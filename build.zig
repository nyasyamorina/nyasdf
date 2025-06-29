const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const nyasdf = b.addStaticLibrary(.{
        .root_source_file = "src/nyasdf.zig",
        .target = target,
        .optimize = optimize,
    });

    const mod_tests = b.addTest(.{ .root_module = nyasdf.root_module });
    const run_mod_tests = b.addRunArtifact(mod_tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_mod_tests.step);
}
