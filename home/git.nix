{ config, pkgs, ... }: {
  programs.git = {
    enable = true;
    userEmail = "29845794+Anomalocaridid@users.noreply.github.com";
    userName = "Anomalocaridid";

    lfs.enable = true;

    delta = {
      enable = true;
      options = {
        navigate = true;
        features = "catppuccin-${config.catppuccin.flavour}";
        # side-by-side = true;
        interactive.keep-plus-minus-markers = false;
        "catppuccin-${config.catppuccin.flavour}" =
          let
            palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
            red20Percent = "483346";
            red60Percent = "9d5477";
            green20Percent = "394545";
            green60Percent = "6f9473";
          in
          {
            commit-decoration-style = "box ul";
            dark = true;
            file-decoration-style = "#${palette.text.hex}";
            file-style = "#${palette.text.hex}";
            hunk-header-decoration-style = "box ul";
            hunk-header-file-style = "bold";
            hunk-header-line-number-style = "bold #${palette.subtext0.hex}";
            hunk-header-style = "file line-number syntax";
            line-numbers = true;
            line-numbers-left-style = "#${palette.overlay0.hex}";
            line-numbers-minus-style = "bold #${palette.red.hex}";
            line-numbers-plus-style = "bold #${palette.green.hex}";
            line-numbers-right-style = "#${palette.overlay0.hex}";
            line-numbers-zero-style = "#${palette.overlay0.hex}";
            # Not exactly standards-conformant,
            # But idk how to make it look good otherwise
            minus-emph-style = "bold syntax #${red60Percent}";
            minus-style = "bold syntax #${red20Percent}";
            plus-emph-style = "bold syntax #${green60Percent}";
            plus-style = "bold syntax #${green20Percent}";
            syntax-theme = "Catppuccin-${config.catppuccin.flavour}";
          };
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
    # Also ensure --side-by-side is only used for git diffs
    # GIT_PAGER = "PAGER='bat --plain' delta --side-by-side";
    GIT_PAGER = "PAGER='bat --plain' delta";
    DELTA_FEATURES = "+side-by-side";
  };
}
