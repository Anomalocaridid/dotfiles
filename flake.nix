{
  description = "My personal dotfiles for NixOS";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.1-1.tar.gz";
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
    ez-configs = {
      url = "github:ehllie/ez-configs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Nix user repository
    nur = {
      url = "github:nix-community/NUR";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };

    # Generate configs
    nixago = {
      url = "github:nix-community/nixago";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Developer environments
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Prolog language server
    swi-lsp-server = {
      url = "github:jamesnvc/lsp_server";
      flake = false;
    };

    # Niri compositor
    # Provides a binary cache, so do not follow inputs
    niri.url = "github:sodiboo/niri-flake";

    # Only used for GRUB theme
    stylix = {
      url = "github:danth/stylix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

    ttf-to-tty = {
      url = "github:Sigmanificient/ttf_to_tty";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Catppuccin Theming
    # Provides a binary cache, so do not follow inputs
    catppuccin.url = "github:catppuccin/nix";

    # Extra Catppuccin themes
    catppuccin-ohmyrepl = {
      url = "github:catppuccin/ohmyrepl";
      flake = false;
    };

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
    # Provides a binary cache, so do not follow inputs
    nix-gaming.url = "github:fufexan/nix-gaming";

    # Spotify customization
    spicetify-nix = {
      url = "github:Gerg-L/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Discord customization
    nixcord = {
      url = "github:KaylorBen/nixcord";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Widget framework
    ignis = {
      url = "github:linkfrg/ignis";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Yazi plugins
    nix-yazi-plugins = {
      # url = "github:lordkekz/nix-yazi-plugins";
      url = "github:Anomalocaridid/nix-yazi-plugins/extraconfig-option";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Fish exercism wrapper
    exercism-cli-fish-wrapper = {
      url = "github:glennj/exercism-cli-fish-wrapper";
      flake = false;
    };

    # Shaders for ghostty terminal
    ghostty-shaders = {
      url = "github:hackr-sh/ghostty-shaders";
      flake = false;
    };
  };

  outputs =
    inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } rec {
      imports = [
        inputs.ez-configs.flakeModule
        inputs.devshell.flakeModule
      ];

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
        args@{
          pkgs,
          inputs',
          ...
        }:
        {
          formatter = pkgs.nixfmt-rfc-style;
          packages = import ./scripts args;
          devshells = import ./devshells {
            inherit pkgs inputs;
          };
        };
    };
}
