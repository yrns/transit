{
  description = "hello, rust";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.utils.url = "github:numtide/flake-utils";
  inputs.rustify.url = "github:yrns/rustify";

  outputs = { self, nixpkgs, utils, rustify }:
    # Cargo.lock is only valid for the current system.
    utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        crateOverrides = rustify.lib.crateOverrides {
          lockFile = ./Cargo.lock;
          inherit pkgs;
        };
        stdenv = pkgs.llvmPackages_latest.stdenv;
        mkShell = pkgs.mkShell.override { inherit stdenv; };
      in
      {
        devShell = with pkgs; mkShell
          rec {
            nativeBuildInputs = [
              bashInteractive
              gnome.zenity
            ] ++ crateOverrides.nativeBuildInputs;
            buildInputs = [ ] ++ crateOverrides.buildInputs;
            LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
            LIBCLANG_PATH = "${llvmPackages_latest.libclang.lib}/lib";
          }
        ;
      });
}
