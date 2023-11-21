{ lib, pkgs, inputs, ... }: {
  # Import all nix files in directory 
  # Should ignore this file and all non-nix files
  imports = map
    (file: ./. + "/${file}")
    (lib.strings.filter
      (file: lib.strings.hasSuffix ".nix" file && file != "default.nix")
      (builtins.attrNames (builtins.readDir ./.))
    );

  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 14d";
    };
    # Set flake inputs to system registry
    registry = lib.mapAttrs
      (_: flake: { inherit flake; })
      # Remove "self" input from registry to not risk messing
      # things up if it is ever used
      (lib.attrsets.removeAttrs inputs [ "self" ]);
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
  };

  networking = {
    hostName = "home-pc";
    networkmanager.enable = true;
  };

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
    # Enable weechat service
    weechat.enable = true;
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
