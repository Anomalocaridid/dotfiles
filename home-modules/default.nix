{
  config,
  lib,
  pkgs,
  osConfig,
  inputs,
  ...
}:
{
  # Import all nix files in directory
  # Should ignore this file and all non-nix files, but keep directories
  imports =
    (
      builtins.readDir ./.
      |> builtins.attrNames
      |> lib.strings.filter (file: file != "default.nix")
      |> map (file: ./. + "/${file}")
    )
    ++ [
      inputs.nix-index-database.hmModules.nix-index
    ];

  home = {
    homeDirectory = "/home/${config.home.username}";
    packages = with pkgs; [
      exercism
      itd
      killall
      orca-slicer # 3D Printer Slicer
      pavucontrol # Graphical audio controller
      qalculate-gtk
      quickemu # Make and run VMs
      strawberry
      tree
      tuxpaint
      wl-clipboard
      yubioath-flutter
    ];

    sessionVariables = {
      # Some things require $EDITOR to be a single command with no args
      EDITOR = "xdg-open";
      VISUAL = "$EDITOR";
      PAGER = "bat";
      MANPAGER = "sh -c 'col --no-backspaces --spaces | bat --plain --language=man'";
    };

    # DON'T TOUCH
    # Use system-level stateVersion
    stateVersion = osConfig.system.stateVersion;
  };

  programs = {
    nix-index.enable = true; # Database for command not found in shell
    home-manager.enable = true; # lets Home Manager manage itself
  };

  services = {
    cliphist.enable = true;
    syncthing = {
      enable = true;
      # TODO: Enable and figure out how to configure declaratively
      # tray.enable = true;
    };
  };

  systemd.user.tmpfiles.rules = [
    # Create a link to /etc/nixos, where the config is in the home directory
    "L ${config.home.homeDirectory}/nixos -   -            -     - /etc/nixos"
    # Ensure SSH keys have proper permissions.
    # NOTE: persistence permissions only seem to apply upon creating a bind mount
    # NOTE: Directory and contents need to have permissions set separately or else it gets set to root permissions for some reason
    "z ${config.home.homeDirectory}/.ssh 0700 ${config.home.username} users - -"
    "Z ${config.home.homeDirectory}/.ssh/* 0600 ${config.home.username} users - -"
  ];
}
