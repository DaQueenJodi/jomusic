pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const cpp_lib = b.addStaticLibrary(.{
        .name = "taglib",
        .optimize = optimize,
        .target = target,
    });

    cpp_lib.linkLibC();
    cpp_lib.linkLibCpp();

    const taglib_dep = b.dependency("taglib", .{});
    for (TAGLIB_INCLUDE_DIRS) |inc| {
        cpp_lib.addIncludePath(taglib_dep.path(inc));
    }
    cpp_lib.addCSourceFiles(.{
        .root = taglib_dep.path("taglib"),
        .files = TAGLIB_SRCS,
        .flags = &.{"-std=c++17"},
    });

    // (header only library)
    const utf8_cpp_dep = b.dependency("utf8-cpp", .{});
    cpp_lib.addIncludePath(utf8_cpp_dep.path("source"));

    const c_lib = b.addStaticLibrary(.{
        .name = "taglib-c",
        .optimize = optimize,
        .target = target,
    });

    c_lib.linkLibrary(cpp_lib);
    c_lib.linkLibC();
    c_lib.linkLibCpp();
    c_lib.addCSourceFile(.{
        .file = taglib_dep.path("bindings/c/tag_c.cpp"),
    });
    for (TAGLIB_INCLUDE_DIRS) |inc| {
        c_lib.addIncludePath(taglib_dep.path(inc));
    }

    const translated_taglib_c = b.addTranslateC(.{
        .root_source_file = taglib_dep.path("bindings/c/tag_c.h"),
        .target = target,
        .optimize = optimize,
    });
    const taglib_c_module = translated_taglib_c.addModule("taglib");
    taglib_c_module.linkLibrary(c_lib);
}

// basically everything below this line is directly stolen from https://github.com/taglib/taglib/blob/f3fb4d83a469fea7e23491a1dfbac14c728ac968/taglib/CMakeLists.txt
const TAGLIB_INCLUDE_DIRS = &[_][]const u8{
    "taglib/",
    "taglib/toolkit",
    "taglib/asf",
    "taglib/mpeg",
    "taglib/ogg",
    "taglib/ogg/flac",
    "taglib/flac",
    "taglib/mpc",
    "taglib/mp4",
    "taglib/ogg/vorbis",
    "taglib/ogg/speex",
    "taglib/ogg/opus",
    "taglib/mpeg/id3v2",
    "taglib/mpeg/id3v2/frames",
    "taglib/mpeg/id3v1",
    "taglib/ape",
    "taglib/wavpack",
    "taglib/trueaudio",
    "taglib/riff",
    "taglib/riff/aiff",
    "taglib/riff/wav",
    "taglib/mod",
    "taglib/s3m",
    "taglib/it",
    "taglib/xm",
    "taglib/dsf",
    "taglib/dsdiff",
};

const MPEG_SRCS = &[_][]const u8{
    "mpeg/mpegfile.cpp",
    "mpeg/mpegproperties.cpp",
    "mpeg/mpegheader.cpp",
    "mpeg/xingheader.cpp",
};

const ID3V1_SRCS = &[_][]const u8{
    "mpeg/id3v1/id3v1tag.cpp",
    "mpeg/id3v1/id3v1genres.cpp",
};

const ID3V2_SRCS = &[_][]const u8{
    "mpeg/id3v2/id3v2framefactory.cpp",
    "mpeg/id3v2/id3v2synchdata.cpp",
    "mpeg/id3v2/id3v2tag.cpp",
    "mpeg/id3v2/id3v2header.cpp",
    "mpeg/id3v2/id3v2frame.cpp",
    "mpeg/id3v2/id3v2footer.cpp",
    "mpeg/id3v2/id3v2extendedheader.cpp",
};

const FRAMES_SRCS = &[_][]const u8{
    "mpeg/id3v2/frames/attachedpictureframe.cpp",
    "mpeg/id3v2/frames/commentsframe.cpp",
    "mpeg/id3v2/frames/eventtimingcodesframe.cpp",
    "mpeg/id3v2/frames/generalencapsulatedobjectframe.cpp",
    "mpeg/id3v2/frames/ownershipframe.cpp",
    "mpeg/id3v2/frames/popularimeterframe.cpp",
    "mpeg/id3v2/frames/privateframe.cpp",
    "mpeg/id3v2/frames/relativevolumeframe.cpp",
    "mpeg/id3v2/frames/synchronizedlyricsframe.cpp",
    "mpeg/id3v2/frames/textidentificationframe.cpp",
    "mpeg/id3v2/frames/uniquefileidentifierframe.cpp",
    "mpeg/id3v2/frames/unknownframe.cpp",
    "mpeg/id3v2/frames/unsynchronizedlyricsframe.cpp",
    "mpeg/id3v2/frames/urllinkframe.cpp",
    "mpeg/id3v2/frames/chapterframe.cpp",
    "mpeg/id3v2/frames/tableofcontentsframe.cpp",
    "mpeg/id3v2/frames/podcastframe.cpp",
};

const OGG_SRCS = &[_][]const u8{
    "ogg/oggfile.cpp",
    "ogg/oggpage.cpp",
    "ogg/oggpageheader.cpp",
    "ogg/xiphcomment.cpp",
};

const VORBIS_SRCS = &[_][]const u8{
    "ogg/vorbis/vorbisfile.cpp",
    "ogg/vorbis/vorbisproperties.cpp",
};

const FLACS_SRCS = &[_][]const u8{
    "flac/flacfile.cpp",
    "flac/flacpicture.cpp",
    "flac/flacproperties.cpp",
    "flac/flacmetadatablock.cpp",
    "flac/flacunknownmetadatablock.cpp",
};

const OGGFLACS_SRCS = &[_][]const u8{
    "ogg/flac/oggflacfile.cpp",
};

const MPC_SRCS = &[_][]const u8{
    "mpc/mpcfile.cpp",
    "mpc/mpcproperties.cpp",
};

const MP4_SRCS = &[_][]const u8{
    "mp4/mp4file.cpp",
    "mp4/mp4atom.cpp",
    "mp4/mp4tag.cpp",
    "mp4/mp4item.cpp",
    "mp4/mp4properties.cpp",
    "mp4/mp4coverart.cpp",
    "mp4/mp4itemfactory.cpp",
};

const APE_SRCS = &[_][]const u8{
    "ape/apetag.cpp",
    "ape/apefooter.cpp",
    "ape/apeitem.cpp",
    "ape/apefile.cpp",
    "ape/apeproperties.cpp",
};

const WAVPACK_SRCS = &[_][]const u8{
    "wavpack/wavpackfile.cpp",
    "wavpack/wavpackproperties.cpp",
};

const SPEEX_SRCS = &[_][]const u8{
    "ogg/speex/speexfile.cpp",
    "ogg/speex/speexproperties.cpp",
};

const OPUS_SRCS = &[_][]const u8{
    "ogg/opus/opusfile.cpp",
    "ogg/opus/opusproperties.cpp",
};

const TRUEAUDIO_SRCS = &[_][]const u8{
    "trueaudio/trueaudiofile.cpp",
    "trueaudio/trueaudioproperties.cpp",
};

const ASF_SRCS = &[_][]const u8{
    "asf/asftag.cpp",
    "asf/asffile.cpp",
    "asf/asfproperties.cpp",
    "asf/asfattribute.cpp",
    "asf/asfpicture.cpp",
};

const RIFF_SRCS = &[_][]const u8{
    "riff/rifffile.cpp",
};

const AIFF_SRCS = &[_][]const u8{
    "riff/aiff/aifffile.cpp",
    "riff/aiff/aiffproperties.cpp",
};

const WAV_SRCS = &[_][]const u8{
    "riff/wav/wavfile.cpp",
    "riff/wav/wavproperties.cpp",
    "riff/wav/infotag.cpp",
};

const MOD_SRCS = &[_][]const u8{
    "mod/modfilebase.cpp",
    "mod/modfile.cpp",
    "mod/modtag.cpp",
    "mod/modproperties.cpp",
};

const S3M_SRCS = &[_][]const u8{
    "s3m/s3mfile.cpp",
    "s3m/s3mproperties.cpp",
};

const IT_SRCS = &[_][]const u8{
    "it/itfile.cpp",
    "it/itproperties.cpp",
};

const XM_SRCS = &[_][]const u8{
    "xm/xmfile.cpp",
    "xm/xmproperties.cpp",
};

const DSF_SRCS = &[_][]const u8{
    "dsf/dsffile.cpp",
    "dsf/dsfproperties.cpp",
};

const DSDIFF_SRCS = &[_][]const u8{
    "dsdiff/dsdifffile.cpp",
    "dsdiff/dsdiffproperties.cpp",
    "dsdiff/dsdiffdiintag.cpp",
};

const TOOLKIT_SRCS = &[_][]const u8{
    "toolkit/tstring.cpp",
    "toolkit/tstringlist.cpp",
    "toolkit/tbytevector.cpp",
    "toolkit/tbytevectorlist.cpp",
    "toolkit/tvariant.cpp",
    "toolkit/tbytevectorstream.cpp",
    "toolkit/tiostream.cpp",
    "toolkit/tfile.cpp",
    "toolkit/tfilestream.cpp",
    "toolkit/tdebug.cpp",
    "toolkit/tpicturetype.cpp",
    "toolkit/tpropertymap.cpp",
    "toolkit/tdebuglistener.cpp",
    "toolkit/tzlib.cpp",
    "toolkit/tversionnumber.cpp",
};

const TAGLIB_SRCS =
    MPEG_SRCS ++ ID3V1_SRCS ++ ID3V2_SRCS ++ FRAMES_SRCS ++ OGG_SRCS ++
    VORBIS_SRCS ++ OGGFLACS_SRCS ++ MPC_SRCS ++ APE_SRCS ++ TOOLKIT_SRCS ++ FLACS_SRCS ++
    WAVPACK_SRCS ++ SPEEX_SRCS ++ TRUEAUDIO_SRCS ++ RIFF_SRCS ++ AIFF_SRCS ++ WAV_SRCS ++
    ASF_SRCS ++ MP4_SRCS ++ MOD_SRCS ++ S3M_SRCS ++ IT_SRCS ++ XM_SRCS ++ OPUS_SRCS ++
    DSF_SRCS ++ DSDIFF_SRCS ++
    &[_][]const u8{
    "tag.cpp",
    "tagunion.cpp",
    "fileref.cpp",
    "audioproperties.cpp",
    "tagutils.cpp",
};

const std = @import("std");
