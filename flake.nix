# DO-NOT-EDIT. This file was auto-generated using github:vic/flake-file.
# Use `nix run .#write-flake` to regenerate it.
{
  description = "My personal dotfiles for NixOS";

  outputs = inputs: import ./outputs.nix inputs;

  nixConfig = {
    extra-substituters = [
      "https://catppuccin.cachix.org"
      "https://nixos-raspberrypi.cachix.org"
      "https://nix-gaming.cachix.org"
    ];
    extra-trusted-public-keys = [
      "catppuccin.cachix.org-1:noG/4HkbhJb+lUAdKrph6LaozJvAeEEZj4N732IysmU="
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
      "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
    ];
  };

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
    catppuccin-userstyles = {
      url = "github:catppuccin/userstyles";
      flake = false;
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    firefox-extensions-declarative = {
      url = "github:firefox-extensions-declarative/firefox-extensions-declarative";
      inputs = {
        flake-file.follows = "flake-file";
        flake-parts.follows = "flake-parts";
        import-tree.follows = "import-tree";
        nixpkgs.follows = "nixpkgs";
      };
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
    hpf-passwd = {
      url = "github:Anomalocaridid/hpf-passwd";
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
    niri-nix.url = "git+https://codeberg.org/BANanaD3V/niri-nix";
    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-monitored = {
      url = "github:ners/nix-monitored";
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
    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi/nixos-unstable";
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
    spicetify-nix = {
      url = "github:Gerg-L/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    swi-lsp-server = {
      url = "github:jamesnvc/lsp_server";
      flake = false;
    };
  };
}
