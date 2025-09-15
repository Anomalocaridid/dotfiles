# DO-NOT-EDIT. This file was auto-generated using github:vic/flake-file.
# Use `nix run .#write-flake` to regenerate it.
{
  description = "My personal dotfiles for NixOS";

  outputs = inputs: import ./outputs.nix inputs;

  inputs = {
    catppuccin = {
      url = "github:catppuccin/nix";
    };
    catppuccin-fractal-wallpapers = {
      flake = false;
      url = "github:psylopneunonym/Catppuccin-Fractal-Wallpapers";
    };
    catppuccin-ohmyrepl = {
      flake = false;
      url = "github:catppuccin/ohmyrepl";
    };
    catppuccin-prismlauncher = {
      flake = false;
      url = "github:catppuccin/prismlauncher";
    };
    catppuccin-userstyles-nix = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "github:different-name/catppuccin-userstyles-nix?rev=b347a087e34ddb4ce645014744b101f217350209";
    };
    devshell = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "github:numtide/devshell";
    };
    disko = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "github:nix-community/disko";
    };
    flake-file = {
      url = "github:vic/flake-file";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };
    ghostty-shaders = {
      flake = false;
      url = "github:hackr-sh/ghostty-shaders";
    };
    home-manager = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "github:nix-community/home-manager";
    };
    ignis = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "github:linkfrg/ignis";
    };
    impermanence = {
      url = "github:nix-community/impermanence";
    };
    import-tree = {
      url = "github:vic/import-tree";
    };
    lix = {
      flake = false;
      url = "https://git.lix.systems/lix-project/lix/archive/main.tar.gz";
    };
    lix-module = {
      inputs = {
        lix = {
          follows = "lix";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "https://git.lix.systems/lix-project/nixos-module/archive/main.tar.gz";
    };
    niri = {
      url = "github:sodiboo/niri-flake";
    };
    nix-gaming = {
      url = "github:fufexan/nix-gaming";
    };
    nix-index-database = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "github:Mic92/nix-index-database";
    };
    nix-yazi-plugins = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "github:lordkekz/nix-yazi-plugins";
    };
    nixago = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "github:nix-community/nixago";
    };
    nixos-facter-modules = {
      url = "github:numtide/nixos-facter-modules";
    };
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    nur = {
      inputs = {
        flake-parts = {
          follows = "flake-parts";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "github:nix-community/NUR";
    };
    spicetify-nix = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "github:Gerg-L/spicetify-nix";
    };
    swi-lsp-server = {
      flake = false;
      url = "github:jamesnvc/lsp_server";
    };
    unify = {
      inputs = {
        flake-parts = {
          follows = "flake-parts";
        };
        home-manager = {
          follows = "home-manager";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      url = "git+https://codeberg.org/quasigod/unify";
    };
  };

}
