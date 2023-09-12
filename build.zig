const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "jomusic",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    const flags = &[_][]const u8{
        "-DMA_NO_FLAC",
        "-DMA_NO_WEBAUDIO",
        "-DMA_NO_ENCODING",
        "-DMA_NO_NULL",
        "--std=c99"
    };
    exe.addCSourceFile(.{.file = .{.cwd_relative = "deps/miniaudio_impl.c"}, .flags = flags});
    exe.addIncludePath(.{.cwd_relative = "deps/"});
    exe.linkLibC();
    exe.linkSystemLibrary("m");
    exe.linkSystemLibrary("pthread");
    exe.linkSystemLibrary("dl");
    exe.linkSystemLibraryPkgConfigOnly("taglib_c");

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
