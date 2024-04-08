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

    hyprlock = {
      url = "github:hyprwm/hyprlock";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hypridle = {
      url = "github:hyprwm/hypridle";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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
    catppuccin.url = "github:catppuccin/nix";

    # Catppuccin port creation tools
    catppuccin-toolbox = {
      url = "github:catppuccin/toolbox";
      inputs.nixpkgs.follows = "nixpkgs";
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

    # Remove when nixpkgs/#297434 is merged
    # Cannot find a way to use this when fetched with nvfetcher.toml
    ly-module-patch = {
      url = "https://github.com/NixOS/nixpkgs/pull/297434.patch";
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
    , hyprlock
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

      channels."nixpkgs".patches = [
        inputs.ly-module-patch
      ];

      sharedOverlays = [
        # custom overlay
        (import ./pkgs)
        # Hyprlock
        # Needed for lockman.sh
        hyprlock.overlays.default
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
