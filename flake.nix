{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.utils.url = "github:numtide/flake-utils";
  inputs.rustify.url = "github:yrns/rustify";

  outputs = {
    self,
    nixpkgs,
    utils,
    rustify,
  }:
  # Cargo.lock is only valid for the current system.
    utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShell = rustify.lib.mkShell {
        lockFile = ./Cargo.lock;
        inherit pkgs;
      };
    });
}
