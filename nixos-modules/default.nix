{
  lib,
  pkgs,
  inputs,
  ...
}:
{
  # Import all nix files in directory 
  # Should ignore this file and all non-nix files
  imports =
    map (file: ./. + "/${file}") (
      lib.strings.filter (file: lib.strings.hasSuffix ".nix" file && file != "default.nix") (
        builtins.attrNames (builtins.readDir ./.)
      )
    )
    ++ [
      inputs.lix-module.nixosModules.default
      inputs.disko.nixosModules.disko
      inputs.home-manager.nixosModules.home-manager
      inputs.impermanence.nixosModules.impermanence
      inputs.stylix.nixosModules.stylix
      inputs.catppuccin.nixosModules.catppuccin
      inputs.nix-gaming.nixosModules.pipewireLowLatency
      inputs.spicetify-nix.nixosModules.spicetify
    ];

  nix = {
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      auto-optimise-store = true;
      repl-overlays = [ ../repl-overlay.nix ]; # Lix-specific setting
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 14d";
    };
    # Set system registry to flake inputs
    registry = lib.pipe inputs [
      # Remove non flake inputs, which cause errors
      # Flakes have an attribute _type, which equals "flake"
      # while non-flakes lack this attribute
      (lib.filterAttrs (_: flake: lib.attrsets.hasAttr "_type" flake))
      (lib.mapAttrs (_: flake: { inherit flake; }))
    ];
    # For some reason, lix needs this to replace the nix command
    package = pkgs.lix;
  };

  nixpkgs = {
    overlays = [
      # custom overlay
      (import ../pkgs)
      # Hyprland community tools
      inputs.hyprland-contrib.overlays.default
    ];
    config.allowUnfree = true;
  };

  # Use GRUB
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_xanmod_latest;
    loader = {
      grub = {
        device = "nodev";
        efiSupport = true;
      };
      efi.canTouchEfiVariables = true;
    };
    # Enable KVM nested virtualization
    extraModprobeConfig = "options kvm_amd nested=1";
  };

  networking.networkmanager.enable = true;

  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # use xkbOptions in tty.
  console.useXkbConfig = true;

  hardware = {
    # Enable QMK support
    keyboard.qmk.enable = true;
    # Enable AMD microcode updates
    enableRedistributableFirmware = true;
  };

  # Enable ssh agent
  programs.ssh.startAgent = true;

  # List services that you want to enable:
  services = {
    # Enable CUPS to print documents.
    printing.enable = true;
    # Required for udiskie
    udisks2.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
