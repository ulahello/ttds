{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-24.11";

  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux.default =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux; in
      pkgs.mkShell.override { stdenv = pkgs.clangStdenv; } {
        buildInputs = with pkgs; [
          systemd.dev meson ninja libdrm pkg-config

          cabal-install ghc zlib

          just
        ];

        CC = "clang";
      };
  };
}
