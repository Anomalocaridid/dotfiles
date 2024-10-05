{
  description = "My personal dotfiles for NixOS";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.90.0.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence.url = "github:nix-community/impermanence";

    # Use nix-index without having to generate the database locally
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Flake framework
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Flake Parts module for defining configs
    ez-configs.url = "github:ehllie/ez-configs";

    # Hyprland community tools
    hyprland-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Only used for GRUB theme
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ttf-to-tty = {
      url = "github:Sigmanificient/ttf_to_tty";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Catppuccin Theming
    catppuccin.url = "github:catppuccin/nix";

    # Catppuccin port creation tools
    catppuccin-catwalk = {
      url = "github:catppuccin/catwalk";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    catppuccin-whiskers = {
      url = "github:catppuccin/whiskers";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Extra Catppuccin themes
    # Change to official repo when catppuccin/prismlauncher#6 is merged
    catppuccin-prismlauncher = {
      url = "github:Anomalocaridid/prismlauncher/whiskers";
      flake = false;
    };

    # Catppuccin wallpapers
    catppuccin-fractal-wallpapers = {
      url = "github:psylopneunonym/Catppuccin-Fractal-Wallpapers";
      flake = false;
    };

    # Gaming tweaks
    nix-gaming.url = "github:fufexan/nix-gaming";

    # Spotify customization
    spicetify-nix = {
      url = "github:Gerg-L/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # yazi plugins
    ## Previewers
    glow-yazi = {
      url = "github:Reledia/glow.yazi";
      flake = false;
    };

    miller-yazi = {
      url = "github:Reledia/miller.yazi";
      flake = false;
    };

    exifaudio-yazi = {
      url = "github:Sonico98/exifaudio.yazi";
      flake = false;
    };

    ouch-yazi = {
      url = "github:ndtoan96/ouch.yazi";
      flake = false;
    };

    ## Functional Plugins
    ### Has smart-filter.yazi and full-border.yazi
    yazi-plugins = {
      url = "github:yazi-rs/plugins";
      flake = false;
    };

    ### Jumping
    relative-motions-yazi = {
      url = "github:dedukun/relative-motions.yazi";
      flake = false;
    };

    ### UI enhancements
    starship-yazi = {
      url = "github:Rolv-Apneseth/starship.yazi";
      flake = false;
    };
  };

  outputs =
    inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } rec {
      imports = [ inputs.ez-configs.flakeModule ];

      ezConfigs = {
        root = ./.;
        globalArgs = {
          inherit inputs;
          inherit (flake) diskoConfigurations;
        };
      };

      # Expose this to use flake directly with Disko
      flake.diskoConfigurations = import ./disko-configurations;

      systems = [ "x86_64-linux" ];

      perSystem =
        args@{ pkgs, inputs', ... }:
        {
          formatter = pkgs.nixfmt-rfc-style;
          packages = import ./scripts args;
        };
    };
}
