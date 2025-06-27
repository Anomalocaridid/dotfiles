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
    (
      builtins.readDir ./.
      |> builtins.attrNames
      |> lib.strings.filter (file: lib.strings.hasSuffix ".nix" file && file != "default.nix")
      |> map (file: ./. + "/${file}")
    )
    ++ [
      inputs.lix-module.nixosModules.default
      inputs.disko.nixosModules.disko
    ];

  nix = {
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
        "pipe-operator" # Lix-specific feature
      ];
      auto-optimise-store = true;
      repl-overlays = [ ../repl-overlay.nix ]; # Lix-specific setting
      trusted-users = [ "anomalocaris" ]; # Fixes issue with not being able to use trusted-public-keys sometimes
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 14d";
    };
    # Set system registry to flake inputs
    # Remove non flake inputs, which cause errors
    # Flakes have an attribute _type, which equals "flake"
    # while non-flakes lack this attribute
    registry =
      inputs
      |> (lib.filterAttrs (_: flake: lib.attrsets.hasAttr "_type" flake))
      |> (lib.mapAttrs (_: flake: { inherit flake; }));
  };

  nixpkgs = {
    overlays = [
      # custom overlay
      (import ../pkgs)
      # Nix user repository
      inputs.nur.overlays.default
      # Yazi plugins
      inputs.nix-yazi-plugins.overlays.default
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

  # List services that you want to enable:
  services = {
    # Enable CUPS to print documents.
    printing.enable = true;
    # Required for udiskie
    udisks2.enable = true;
    # Required for ignis
    gvfs.enable = true;
  };

  # Tell electron apps to use Wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # Ensure that nixos config has proper permissions
  # NOTE: persistence permissions only seem to apply upon creating a bind mount
  systemd.tmpfiles.rules = [ "Z /etc/nixos - anomalocaris users -" ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
