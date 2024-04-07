{ pkgs, ... }: {
  programs.git = {
    enable = true;
    userEmail = "29845794+Anomalocaridid@users.noreply.github.com";
    userName = "Anomalocaridid";

    lfs.enable = true;

    delta = {
      enable = true;
      catppuccin.enable = true;
      options = {
        navigate = true;
        interactive.keep-plus-minus-markers = false;
      };
    };

    extraConfig = {
      init.defaultBranch = "main";
      core.autocrlf = "input";
      merge.conflictstyle = "diff3";
      diff.colorMoved = "default";
    };
  };

  programs.lazygit = {
    enable = true;
    catppuccin.enable = true;
    settings = {
      theme.nerdFontsVersion = 3;
      update.method = false;
      disableStartupPopups = true;
    };
  };

  home.sessionVariables = {
    # Ensure bat's line numbers don't show up and mess things up
    DELTA_PAGER = "bat --plain";
    # Ensure --side-by-side is only used for git diffs
    DELTA_FEATURES = "+side-by-side";
  };
}
