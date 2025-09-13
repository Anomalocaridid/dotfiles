{ inputs, ... }:
{
  imports = [
    inputs.devshell.flakeModule
    inputs.disko.flakeModule
    inputs.flake-file.flakeModules.default
    inputs.flake-parts.flakeModules.modules
    inputs.unify.flakeModule
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

      # Generate configs
      nixago = {
        url = "github:nix-community/nixago";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      # Hardware feature detection
      nixos-facter-modules.url = "github:numtide/nixos-facter-modules";

      # Configuration framework
      unify = {
        url = "git+https://codeberg.org/quasigod/unify";
        inputs = {
          nixpkgs.follows = "nixpkgs";
          home-manager.follows = "home-manager";
          flake-parts.follows = "flake-parts";
        };
      };
    };
  };
}
