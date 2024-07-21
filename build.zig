pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const llvm = b.option(bool, "llvm", "") orelse false;
    const exe = b.addExecutable(.{
        .name = "jomusic",
        .root_source_file = b.path("main.zig"),
        .optimize = optimize,
        .target = target,
        .use_llvm = llvm,
        .use_lld = llvm,
    });
    b.installArtifact(exe);

    const run_exe = b.addRunArtifact(exe);
    if (b.args) |args| run_exe.addArgs(args);
    const run_exe_step = b.step("run", "");
    run_exe_step.dependOn(&run_exe.step);

    exe.linkLibC();

    exe.linkSystemLibrary("pulse");
    exe.linkSystemLibrary("pulse-simple");

    exe.addCSourceFile(.{
        .file = b.path("vendor/all_the_audio_guys_impl.c"),
        .flags = &.{"-std=c11"},
    });
    exe.addIncludePath(b.path(""));

    const taglib_dep = b.dependency("taglib", .{ .optimize = optimize, .target = target });
    exe.root_module.addImport("taglib", taglib_dep.module("taglib"));

    const sqlite_dep = b.dependency("sqlite", .{});
    exe.root_module.addImport("sqlite", sqlite_dep.module("sqlite"));
    exe.linkLibrary(sqlite_dep.artifact("sqlite"));

    const docs_path = exe.getEmittedDocs();
    const serve_docs = b.addSystemCommand(&.{ "python", "-m", "http.server", "-d" });
    serve_docs.addDirectoryArg(docs_path);

    const docs_step = b.step("docs", "");
    docs_step.dependOn(&serve_docs.step);
}

const std = @import("std");
