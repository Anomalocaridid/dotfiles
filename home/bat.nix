{ pkgs, ... }: {
  programs.bat = {
    enable = true;
    config = {
      theme = "cyberpunk-neon";
    };
    themes = {
      cyberpunk-neon = builtins.readFile ./.config/bat/themes/cyberpunk-neon.tmTheme;
    };
    extraPackages = with pkgs.bat-extras; [
      batdiff
      batgrep
      batman
      batpipe
    ];
  };

  # Designate Unicode Private Use Areas as printable characters
  # Needed for Nerd Fonts icons to display properly
  home.sessionVariables.LESSUTFCHARDEF = "E000-F8FF:p,F0000-FFFFD:p,100000-10FFFD:p";

  systemd.user.services.bat-cache = {
    Unit.Description = "Build and update bat cache";
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.bat}/bin/bat cache --build";
    };
    Install.WantedBy = [ "default.target" ];
  };
}
