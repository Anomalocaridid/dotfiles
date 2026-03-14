# DO-NOT-EDIT. This file was auto-generated using github:vic/flake-file.
# Use `nix run .#write-flake` to regenerate it.
{
  description = "My personal dotfiles for NixOS";

  outputs = inputs: import ./outputs.nix inputs;

  inputs = {
    cascade = {
      url = "github:cascadefox/cascade";
      flake = false;
    };
    catppuccin.url = "github:catppuccin/nix";
    catppuccin-fractal-wallpapers = {
      url = "github:psylopneunonym/Catppuccin-Fractal-Wallpapers";
      flake = false;
    };
    catppuccin-ohmyrepl = {
      url = "github:catppuccin/ohmyrepl";
      flake = false;
    };
    catppuccin-userstyles-nix = {
      url = "github:different-name/catppuccin-userstyles-nix?rev=b347a087e34ddb4ce645014744b101f217350209";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
    ghostty-shaders = {
      url = "github:hackr-sh/ghostty-shaders";
      flake = false;
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ignis = {
      url = "github:linkfrg/ignis";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence = {
      url = "github:nix-community/impermanence";
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };
    import-tree.url = "github:vic/import-tree";
    niri.url = "github:sodiboo/niri-flake";
    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-yazi-plugins = {
      url = "github:lordkekz/nix-yazi-plugins";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixago = {
      url = "github:nix-community/nixago";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixcord = {
      url = "github:kaylorben/nixcord";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
    };
    nixos-facter-modules.url = "github:numtide/nixos-facter-modules";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
    spicetify-nix = {
      url = "github:Gerg-L/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    swi-lsp-server = {
      url = "github:jamesnvc/lsp_server";
      flake = false;
    };
    unify = {
      url = "git+https://codeberg.org/quasigod/unify";
      inputs = {
        flake-parts.follows = "flake-parts";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };
}
