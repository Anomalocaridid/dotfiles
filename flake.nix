{
  description = "My personal dotfiles for NixOS";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.90.0.tar.gz";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
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
    ### Jumping
    relative-motions-yazi = {
      url = "github:dedukun/relative-motions.yazi/0.2.5";
      flake = false;
    };

    ### filter enhancements
    # Has smart-filter.yazi
    yazi-plugins = {
      url = "github:yazi-rs/plugins";
      flake = false;
    };

    ### UI enhancements
    starship-yazi = {
      url = "github:Rolv-Apneseth/starship.yazi/6197e4cca4caed0121654079151632f6abcdcae9";
      flake = false;
    };

    ## Utilities
    icons-brew-yazi = {
      url = "github:lpnh/icons-brew.yazi";
      flake = false;
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      lix-module,
      disko,
      home-manager,
      impermanence,
      nix-index-database,
      flake-utils,
      hyprland-contrib,
      stylix,
      catppuccin,
      nix-gaming,
      spicetify-nix,
      ...
    }:
    flake-utils.lib.mkFlake rec {
      inherit self inputs;

      channelsConfig.allowUnfree = true;

      # channels."nixpkgs".patches = [ ];

      sharedOverlays = [
        # custom overlay
        (import ./pkgs)
        # Hyprland community tools
        hyprland-contrib.overlays.default
      ];

      hostDefaults = {
        system = flake-utils.lib.system.x86_64-linux;
        modules = [
          ./modules
          lix-module.nixosModules.default
          disko.nixosModules.disko
          home-manager.nixosModules.home-manager
          impermanence.nixosModules.impermanence
          stylix.nixosModules.stylix
          catppuccin.nixosModules.catppuccin
          nix-gaming.nixosModules.pipewireLowLatency
          spicetify-nix.nixosModules.spicetify
        ];
      };

      hosts.home-pc.modules = [
        ./hosts/home-pc/configuration.nix
        diskoConfigurations.home-pc
      ];

      # Expose this to use flake directly with Disko
      diskoConfigurations.home-pc = (
        import ./disko-config.nix {
          disk = "/dev/nvme0n1";
          memory = "32G";
        }
      );

      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;
    };
}
