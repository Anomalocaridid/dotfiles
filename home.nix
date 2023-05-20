{ lib, pkgs, osConfig, ... }: {
  # Import all files in ./home/
  # Note: Will fail to build if non-nix files are present in ./home/
  imports = map (n: ./. + "/home/${n}") (builtins.attrNames (builtins.readDir ./home));

  home = rec {
    username = "anomalocaris";
    homeDirectory = "/home/${username}";
    packages = with pkgs; [
      neofetch
      tree
      libreoffice-fresh
      strawberry
      nyxt
      freetube
      pavucontrol # Graphical audio controller
      (nerdfonts.override { fonts = [ "FiraCode" ]; })
      advcpmv-coreutils # add progress bars to cp and mv (depends on overlay in flake.nix)
      firefox # fallback browser
    ];

    sessionVariables = {
      EDITOR = "handlr open";
      VISUAL = "$EDITOR";
      PAGER = "bat";
      MANPAGER = "sh -c 'col --no-backspaces --spaces | bat --plain --language=man'";
    };

    activation = {
      symlinkConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] #shell
        ''
          ln --symbolic --force --no-target-directory "/etc/nixos" ${homeDirectory}/nixos
          # chown -R ${username}:users /persist/etc/nixos
        '';
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
    nix-index.enable = true;
    dircolors.enable = true;
    # lets Home Manager manage itself
    home-manager.enable = true;
  };
}
