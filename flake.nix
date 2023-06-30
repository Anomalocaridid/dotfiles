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

    # Nix User Repo
    nur.url = "github:nix-community/NUR";

    # Hyprland
    hyprland.url = "github:hyprwm/Hyprland";

    hyprland-autoname-patch = {
      url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/236515.patch";
      flake = false;
    };

    # Elkowar's Wacky Widgets
    eww = {
      url = "github:elkowar/eww";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        rust-overlay.follows = "rust-overlay";
      };
    };

    # Needed for eww
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Up to date Unison packages
    unison-nix = {
      url = "github:ceedubs/unison-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Fork of zsh-bd zsh plugin
    zsh-bd = {
      url = "github:mawkler/zsh-bd";
      flake = false;
    };

    # Hooks to simplify zle widget code
    zsh-hooks = {
      url = "github:zsh-hooks/zsh-hooks";
      flake = false;
    };

    # Nyxt plugins
    nx-freestance-handler = {
      url = "github:kssytsrk/nx-freestance-handler";
      flake = false;
    };

    nx-fruit = {
      url = "github:atlas-engineer/nx-fruit";
      flake = false;
    };

    nx-kaomoji = {
      url = "github:aartaka/nx-kaomoji";
      flake = false;
    };

    nx-search-engines = {
      url = "github:aartaka/nx-search-engines";
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
    , nur
    , hyprland
    , eww
    , rust-overlay
    , unison-nix
    , ...
    }: flake-utils.lib.mkFlake rec {
      inherit self inputs;

      channelsConfig.allowUnfree = true;

      # TODO: remove when merged
      channels."nixpkgs".patches = [ inputs.hyprland-autoname-patch ];

      sharedOverlays = [
        # custom overlay
        (import ./pkgs)
        # Eww master branch
        eww.overlays.default
        rust-overlay.overlays.default
        # Up to date Unison packages
        unison-nix.overlay
      ];

      hostDefaults = {
        system = flake-utils.lib.system.x86_64-linux;
        modules = [
          ./modules
          disko.nixosModules.disko
          nur.nixosModules.nur
          home-manager.nixosModules.home-manager
          impermanence.nixosModules.impermanence
          hyprland.nixosModules.default
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
