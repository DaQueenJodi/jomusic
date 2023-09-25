{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-05";
    zig-overlay = "github:mitchellh/zig-overlay";
    zls = "github:zigtools/zls";
  };
  {self, ...}@inputs:
  {
    devShells."x86_64-linux".default = stdenv.mkDerivation {
      packages = with pkgs; [
        pkg-config
        taglib
        zlib
        sqlite
        libpulseaudio
        readline

        (zls.packages.x86-linux.default)
        (zig-overlay.packages.x86-linux.master-2023-09-09)
      ];
    };
  }
}
