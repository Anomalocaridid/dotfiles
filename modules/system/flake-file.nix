{ inputs, ... }:
{
  imports = [
    inputs.devshell.flakeModule
    inputs.disko.flakeModule
    inputs.flake-file.flakeModules.default
    inputs.flake-parts.flakeModules.modules
  ];

  flake-file = {
    description = "My personal dotfiles for NixOS";

    inputs = {
      devshell = {
        url = "github:numtide/devshell";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      disko = {
        url = "github:nix-community/disko";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      flake-file.url = "github:vic/flake-file";

      flake-parts.url = "github:hercules-ci/flake-parts";

      import-tree.url = "github:vic/import-tree";
    };
  };
}
