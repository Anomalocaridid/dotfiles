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

    flake-parts.url = "github:hercules-ci/flake-parts";

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
    inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      { withSystem, ... }:
      {
        flake =
          let
            shared = [
              ./modules
              inputs.lix-module.nixosModules.default
              inputs.disko.nixosModules.disko
              inputs.home-manager.nixosModules.home-manager
              inputs.impermanence.nixosModules.impermanence
              inputs.stylix.nixosModules.stylix
              inputs.catppuccin.nixosModules.catppuccin
              inputs.nix-gaming.nixosModules.pipewireLowLatency
              inputs.spicetify-nix.nixosModules.spicetify
            ];
          in
          rec {
            nixosConfigurations.home-pc = withSystem "x86_64-linux" (
              { pkgs, system, ... }:
              nixpkgs.lib.nixosSystem {
                inherit system;
                modules = [
                  ./hosts/home-pc/configuration.nix
                  diskoConfigurations.home-pc
                ] ++ shared;
                specialArgs = {
                  inherit inputs pkgs;
                };
              }
            );

            # Expose this to use flake directly with Disko
            diskoConfigurations.home-pc = (
              import ./disko-config.nix {
                disk = "/dev/nvme0n1";
                memory = "32G";
              }
            );
          };

        systems = [ "x86_64-linux" ];

        perSystem =
          { pkgs, system, ... }:
          {
            # Apply overlay and allow unfree packages
            _module.args.pkgs = import nixpkgs {
              inherit system;
              overlays = [
                # custom overlay
                (import ./pkgs)
                # Hyprland community tools
                inputs.hyprland-contrib.overlays.default
              ];
              config.allowUnfree = true;
            };

            formatter = pkgs.nixfmt-rfc-style;
          };
      }
    );
}
