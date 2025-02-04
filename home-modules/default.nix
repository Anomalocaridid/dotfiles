{
  lib,
  pkgs,
  config,
  osConfig,
  ...
}:
{
  # Import all nix files in directory
  # Should ignore this file and all non-nix files
  # Currently, all non-nix files and dirs here are hidden dotfiles
  imports = map (file: ./. + "/${file}") (
    lib.strings.filter (file: !lib.strings.hasPrefix "." file && file != "default.nix") (
      builtins.attrNames (builtins.readDir ./.)
    )
  );

  home = {
    homeDirectory = "/home/${config.home.username}";
    packages = with pkgs; [
      exercism
      itd
      keepassxc
      killall
      pavucontrol # Graphical audio controller
      qalculate-gtk
      # TODO: Uncomment when NixOS/nixpkgs/pull/369622 makes it to unstable
      # quickemu # Make and run VMs
      strawberry
      tree
      tuxpaint
      ventoy
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
    dircolors.enable = true; # Color ls output
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

  systemd.user.tmpfiles.rules = [ "L ${config.home.homeDirectory}/nixos - - - - /etc/nixos" ];
}
