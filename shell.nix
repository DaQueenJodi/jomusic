{ pkgs ? import <nixpkgs> {} }: with pkgs;
mkShell.override {
  stdenv = clang15Stdenv;
} {
  packages = [
    (builtins.getFlake "github:zigtools/zls").packages.x86_64-linux.default
    (builtins.getFlake "github:mitchellh/zig-overlay").packages.x86_64-linux.master-2023-09-09
    pkg-config
    taglib
    zlib # dep for taglib
  ];
}
