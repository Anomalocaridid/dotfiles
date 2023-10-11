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

    # Gaming tweaks
    nix-gaming.url = "github:fufexan/nix-gaming";

    # Slippi
    ssbm-nix.url = "github:djanatyn/ssbm-nix";

    # Spotify customization
    spicetify-nix = {
      url = "github:the-argus/spicetify-nix";
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
    , nix-gaming
    , ssbm-nix
    , spicetify-nix
    , unison-nix
    , ...
    }: flake-utils.lib.mkFlake rec {
      inherit self inputs;

      channelsConfig.allowUnfree = true;

      sharedOverlays = [
        # custom overlay
        (import ./pkgs)
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
