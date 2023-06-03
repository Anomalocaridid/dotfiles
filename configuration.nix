{ pkgs, ... }: {
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

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
  };

  boot = {
    # Enable KVM nested virtualization
    extraModprobeConfig = "options kvm_amd nested=1";
    # Use GRUB
    loader = {
      grub = {
        device = "nodev";
        efiSupport = true;
        theme = pkgs.callPackage ./pkgs/cyberre-grub-theme { };
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
  console = {
    font = "FiraCode Nerd Font";
    colors = [
      # Normal
      "000b1e"
      "ff0000"
      "d300c4"
      "f57800"
      "123e7c"
      "711c91"
      "0abdc6"
      "0abdc6"

      # Bright
      "1c61c2"
      "ff0000"
      "d300c4"
      "f57800"
      "00ff00"
      "711c91"
      "0abdc6"
      "d7d7d5"
    ];
    #   keyMap = "us";
    useXkbConfig = true; # use xkbOptions in tty.
  };


  hardware = {
    # Disable pulseaudio in order to use pipewire
    pulseaudio.enable = false;
    # Enable bluetooth
    bluetooth.enable = true;
    # Enable QMK support
    keyboard.qmk.enable = true;
  };

  systemd = {
    # Ensure certain directories have necessary permissions
    tmpfiles.rules =
      let
        user = "anomalocaris";
        persistDir = "/persist";
      in
      [
        "Z ${persistDir}/etc/nixos    -    ${user} users"
        "d ${persistDir}/home/${user} 0755 ${user} users"
      ];
    services = {
      # Create ClamAV signature database if it does not exist.
      freshclam-init =
        let
          clamavServices = [ "clamav-daemon.service" ];
        in
        {
          description = "Create ClamAV signature database";
          before = clamavServices;
          script = "${pkgs.clamav}/bin/freshclam";
          unitConfig.ConditionPathExists = "!/var/lib/clamav";
          serviceConfig.Type = "oneshot";
          requiredBy = clamavServices;
        };

      # Create btrbk snapshot directory if it doesn't exist yet.
      btrbk-daily-init =
        let
          btrbkServices = [ "btrbk-daily.service" ];
          snapshotDir = "/persist/btrbk_snapshots";
        in
        {
          description = "Ensure btrbk snapshot dir exists";
          before = btrbkServices;
          script = "mkdir ${snapshotDir}";
          unitConfig.ConditionPathExists = "!${snapshotDir}";
          serviceConfig.Type = "oneshot";
          requiredBy = btrbkServices;
        };
    };
  };

  # /persist is needed for boot because it contains password hashes
  # TODO: See if this can be moved to disko config
  fileSystems."/persist".neededForBoot = true;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users =
    let
      passwordDir = "/persist/passwords";
    in
    {
      anomalocaris = {
        shell = pkgs.zsh;
        isNormalUser = true;
        extraGroups = [
          "wheel" # Enable ‘sudo’ for the user
          "libvirtd" # Allow access to virt-manager
        ];
        passwordFile = "${passwordDir}/anomalocaris";
      };
      root.passwordFile = "${passwordDir}/root";
    };

  # Enable libvirtd for virt-manager
  virtualisation.libvirtd.enable = true;

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs;
    [
      virt-manager
    ];

  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      # Enable insults in sudo
      (final: prev: {
        sudo = prev.sudo.override {
          withInsults = true;
        };
      })
    ];
  };

  security = {
    sudo.extraConfig = #sudo
      ''
        # Prevents sudo lecture from appearing after reboot without persisting
        Defaults lecture = never
        # Sudo insults after failed attempts because why not
        Defaults insults
      '';
    # Needed for pipewire
    rtkit.enable = true;
  };

  programs = {
    # Needed for root to access bind mounted dirs created by impermanence
    fuse.userAllowOther = true;
    # Need to enable zsh at system level to use as shell
    zsh.enable = true;
    # Enable dconf for virt-manager
    # NOTE: Autoconnecting to QEMU is configured via dconf in home.nix
    dconf.enable = true;
  };

  # Configure Qt theme
  qt = rec {
    enable = true;
    platformTheme = "gtk2";
    style = platformTheme;
  };

  # List services that you want to enable:
  services = {
    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      # Enable the GNOME Desktop Environment.
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
      # Configure keymap in X11
      layout = "us";
    };
    # Enable CUPS to print documents.
    printing.enable = true;
    # Enable sound.
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
      jack.enable = true;
    };
    # Bluetooth manager
    blueman.enable = true;
    # Enable ClamAV
    clamav = {
      daemon.enable = true;
      updater.enable = true;
    };
    # Btrfs snapshots
    btrbk.instances.daily.settings = {
      snapshot_preserve = "14d";
      snapshot_preserve_min = "2d";
      volume."/persist" = {
        subvolume = ".";
        snapshot_dir = "btrbk_snapshots";
      };
    };
    # Enable weechat service
    weechat.enable = true;
    # Enable Yubikey support
    pcscd.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
