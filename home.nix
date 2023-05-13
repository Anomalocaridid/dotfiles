{ lib, pkgs, osConfig, inputs, ... }: {
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
      imv
      mpv
      nyxt
      freetube
      pavucontrol # Graphical audio controller
      (nerdfonts.override { fonts = [ "FiraCode" ]; })
      # add progress bars to cp and mv
      (coreutils.overrideAttrs
        (oldAttrs: {
          patches = (oldAttrs.patches or [ ]) ++ [ "${inputs.advcpmv}/advcpmv-0.9-${oldAttrs.version}.patch" ];
        }))
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

  programs = {
    nix-index.enable = true;
    dircolors.enable = true;
    # lets Home Manager manage itself
    home-manager.enable = true;
  };
}
