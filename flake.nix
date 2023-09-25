{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
    zig-overlay.url = "github:mitchellh/zig-overlay";
    zls.url = "github:zigtools/zls";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    ...
  }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in
  {
    devShells.x86_64-linux.default = pkgs.mkShell {
      packages = with pkgs; [
        pkg-config
        taglib
        zlib
        sqlite
        libpulseaudio
        readline
        zig
        zls
      ];
    };
  };
}
