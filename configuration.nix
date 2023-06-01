# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

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

  # Use GRUB
  boot.loader = {
    grub = {
      device = "nodev";
      efiSupport = true;
      theme = pkgs.callPackage ./pkgs/cyberre-grub-theme { };
    };
    efi.canTouchEfiVariables = true;
  };

  networking.hostName = "home-pc"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

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

  # Enable the X11 windowing system.
  services.xserver.enable = true;


  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;


  # Configure keymap in X11
  services.xserver.layout = "us";
  # services.xserver.xkbOptions = {
  #   "eurosign:e";
  #   "caps:escape" # map caps to escape.
  # };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
    jack.enable = true;
  };

  # Enable bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Enable ClamAV
  services.clamav = {
    daemon.enable = true;
    updater.enable = true;
  };

  systemd.services.freshclam-init =
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

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Btrfs snapshots
  services.btrbk.instances.daily.settings = {
    snapshot_preserve = "14d";
    snapshot_preserve_min = "2d";
    volume."/persist" = {
      subvolume = ".";
      snapshot_dir = "btrbk_snapshots";
    };
  };

  systemd.services.btrbk-daily-init =
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

  # Enable weechat service
  services.weechat.enable = true;

  # Enable Yubikey support
  services.pcscd.enable = true;

  fileSystems."/persist".neededForBoot = true;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users = {
    anomalocaris = {
      shell = pkgs.zsh;
      isNormalUser = true;
      extraGroups = [
        "wheel" # Enable ‘sudo’ for the user
        "libvirtd" # Allow access to virt-manager
      ];
      passwordFile = "/persist/passwords/anomalocaris";
    };
    root.passwordFile = "/persist/passwords/root";
  };

  # Virtual machine config
  # NOTE: Autoconnecting to QEMU is configured via dconf in home.nix
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  boot.extraModprobeConfig = "options kvm_amd nested=1";

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs;
    [
      virt-manager
      # qemu
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

  security.sudo.extraConfig = ''
    # Prevents sudo lecture from appearing after reboot
    Defaults lecture = never
    # Sudo insults after failed attempts because why not
    Defaults insults
  '';

  programs = {
    # Needed for root to access bind mounted dirs created by impermanence
    fuse.userAllowOther = true;
    # Need to enable zsh at system level
    zsh.enable = true;
  };

  # Ensure certain directories have necessary permissions
  systemd.tmpfiles.rules = [
    "Z /persist/etc/nixos         -    anomalocaris users"
    "d /persist/home/anomalocaris 0755 anomalocaris users"
  ];

  # Configure Qt theme
  qt = rec {
    enable = true;
    platformTheme = "gtk2";
    style = platformTheme;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}

