{
  description = "My personal dotfiles for NixOS";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

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

    flake-utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

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
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };

    # Catppuccin Theming
    catppuccin = {
      url = "github:Stonks3141/ctp-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Unofficial Catppuccin ports
    catppuccin-swaylock = {
      url = "github:remiposo/swaylock";
      flake = false;
    };

    catppuccin-wofi = {
      url = "github:quantumfate/wofi";
      flake = false;
    };

    # Gaming tweaks
    nix-gaming.url = "github:fufexan/nix-gaming";

    # Slippi
    ssbm-nix.url = "github:djanatyn/ssbm-nix";

    # Spotify customization
    spicetify-nix = {
      url = "github:the-argus/spicetify-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    # Up to date Unison packages
    unison-nix = {
      url = "github:ceedubs/unison-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    # Fish plugins
    fish-bd = {
      url = "github:0rax/fish-bd";
      flake = false;
    };

    plugin-sudope = {
      url = "github:oh-my-fish/plugin-sudope";
      flake = false;
    };

    you-should-use = {
      url = "github:paysonwallach/fish-you-should-use";
      flake = false;
    };

    # Nyxt plugins
    nx-fruit = {
      url = "github:atlas-engineer/nx-fruit";
      flake = false;
    };

    nx-search-engines = {
      url = "github:aartaka/nx-search-engines";
      flake = false;
    };

    nx-router = {
      url = "github:migalmoreno/nx-router";
      flake = false;
    };
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , disko
    , home-manager
    , impermanence
    , nix-index-database
    , flake-utils
    , hyprland-contrib
    , stylix
    , catppuccin
    , nix-gaming
    , ssbm-nix
    , spicetify-nix
    , unison-nix
    , ...
    }: flake-utils.lib.mkFlake rec {
      inherit self inputs;

      channelsConfig.allowUnfree = true;

      # channels."nixpkgs".patches = [];

      sharedOverlays = [
        # custom overlay
        (import ./pkgs)
        # Hyprland community tools
        hyprland-contrib.overlays.default
        # Up to date Unison packages
        unison-nix.overlay
      ];

      hostDefaults = {
        system = flake-utils.lib.system.x86_64-linux;
        modules = [
          ./modules
          disko.nixosModules.disko
          home-manager.nixosModules.home-manager
          impermanence.nixosModules.impermanence
          stylix.nixosModules.stylix
          catppuccin.nixosModules.catppuccin
          nix-gaming.nixosModules.pipewireLowLatency
          nix-gaming.nixosModules.steamCompat
          ssbm-nix.nixosModule
          spicetify-nix.nixosModules.spicetify
        ];
      };

      hosts.home-pc.modules = [
        ./hosts/home-pc/configuration.nix
        diskoConfigurations.home-pc
      ];

      # Expose this to use flake directly with Disko
      diskoConfigurations.home-pc = (import ./disko-config.nix {
        disk = "/dev/nvme0n1";
        memory = "32G";
      });
    };
}
