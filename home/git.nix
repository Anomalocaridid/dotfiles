{ config, ... }:
{
  programs.git = {
    enable = true;
    userEmail = "29845794+Anomalocaridid@users.noreply.github.com";
    userName = "Anomalocaridid";

    delta = {
      enable = true;
      options = {
        navigate = true;
        features = "base16";
        side-by-side = true;
        interactive.keep-plus-minus-markers = false;
        base16 = with config.lib.stylix.scheme.withHashtag; {
          commit-decoration-style = "box ul";
          dark = true;
          file-decoration-style = "none";
          file-style = "${base09} ul";
          hunk-header-decoration-style = "box ul";
          hunk-header-file-style = "bold ${base05}";
          hunk-header-line-number-style = "bold ${base09}";
          hunk-header-style = "file line-number syntax";
          line-numbers = true;
          line-numbers-left-style = "${base03}";
          line-numbers-minus-style = "bold ${base08}";
          line-numbers-plus-style = "bold ${base0B}";
          line-numbers-right-style = "${base03}";
          line-numbers-zero-style = "${base03}";
          # Not exactly standards-conformant,
          # But idk how to make it look good otherwise
          minus-emph-style = "bold syntax ${base08}";
          minus-style = "bold syntax #330000";
          plus-emph-style = "bold syntax ${base0B}";
          plus-style = "bold syntax ${base0E}";
          # TODO: Requires bat
          #syntax-theme = "base16";
        };
      };
    };

    extraConfig = {
      init.defaultBranch = "main";
      core.autocrlf = true;
      merge.conflictstyle = "diff3";
      diff.colorMoved = "default";
    };
  };
}