{ lib, pkgs, osConfig, ... }: rec {
  # Import all nix files in directory 
  # Should ignore this file and all non-nix files
  imports = map
    (file: ./. + "/${file}")
    (lib.strings.filter
      (file: lib.strings.hasSuffix ".nix" file && file != "default.nix")
      (builtins.attrNames (builtins.readDir ./.))
    );

  home = rec {
    username = "anomalocaris";
    homeDirectory = "/home/${username}";
    packages = with pkgs; [
      exercism
      firefox # fallback browser
      freetube
      gimp-with-plugins
      keepassxc
      killall
      neofetch
      nerdfonts
      pavucontrol # Graphical audio controller
      qalculate-gtk
      strawberry
      tree
      ventoy
      webcord-vencord
      wl-clipboard
      yubioath-flutter
      zotero
      fup-repl
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
