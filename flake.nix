{
  description = "hello, rust";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.utils.url = "github:numtide/flake-utils";
  # inputs.rustify.url = "github:yrns/rustify";
  inputs.rustify.url = "path:/home/al/src/rustify";

  outputs = { self, nixpkgs, utils, rustify }:
    # Cargo.lock is only valid for the current system.
    utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        crates = rustify.lib.mkShell {
          lockFile = ./Cargo.lock;
          inherit pkgs;
        };
      in {
        devShell = with pkgs;
          mkShell.override { stdenv = clangStdenv; } {
            inputsFrom = [ crates ];
            nativeBuildInputs = [ bashInteractive gnome.zenity ];

            # move this
            CARGO_TARGET_DIR = "/mnt/scrap/target";
          };
      });
}
