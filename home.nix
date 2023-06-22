{ lib, pkgs, osConfig, ... }: rec {
  # Import all files in ./home/
  # Note: Will fail to build if non-nix files are present in ./home/
  imports = map (n: ./. + "/home/${n}") (builtins.attrNames (builtins.readDir ./home));

  home = rec {
    username = "anomalocaris";
    homeDirectory = "/home/${username}";
    packages = with pkgs; [
      exercism
      freetube
      freetube
      gimp-with-plugins
      keepassxc
      killall
      neofetch
      pavucontrol # Graphical audio controller
      qalculate-gtk
      strawberry
      tree
      ventoy
      wl-clipboard
      yubioath-flutter
      zotero
      (nerdfonts.override { fonts = [ "FiraCode" ]; })
      # advcpmv-coreutils # add progress bars to cp and mv (depends on overlay in flake.nix)
      firefox # fallback browser
    ];

    sessionVariables = {
      EDITOR = "handlr open";
      VISUAL = "$EDITOR";
      PAGER = "bat";
      MANPAGER = "sh -c 'col --no-backspaces --spaces | bat --plain --language=man'";
    };

    # DON'T TOUCH
    # Use system-level stateVersion
    stateVersion = osConfig.system.stateVersion;
  };

  # Set qemu as hypervisor for virt-manager
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = rec {
      uris = lib.hm.gvariant.mkArray "s" [ "qemu:///system" ];
      autoconnect = uris;
    };
  };

  programs = {
    nix-index.enable = true; # Database for command not found in shell
    dircolors.enable = true; # Color ls output
    home-manager.enable = true; # lets Home Manager manage itself
  };

  services.syncthing = {
    enable = true;
    # Reenable after getting a tray service
    # tray.enable = true;
  };

  systemd.user.tmpfiles.rules = [
    "L ${home.homeDirectory}/nixos - - - - /etc/nixos"
  ];
}
