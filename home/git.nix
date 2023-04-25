{ ... }:
{
  programs.git = {
    enable = true;
    userEmail = "29845794+Anomalocaridid@users.noreply.github.com";
    userName = "Anomalocaridid";

    delta = {
      enable = true;
      options = {
        navigate = true;
        features = "cyberpunk-neon";
        side-by-side = true;
        interactive.keep-plus-minus-markers = false;
        cyberpunk-neon = {
          commit-decoration-style = "box ul";
          dark = true;
          file-decoration-style = "#ea00d9";
          file-style = "#ea00d9";
          hunk-header-decoration-style = "box ul";
          hunk-header-file-style = "bold";
          hunk-header-line-number-style = "bold #f57800";
          hunk-header-style = "file line-number syntax";
          line-numbers = true;
          line-numbers-left-style = "#ea00d9";
          line-numbers-minus-style = "bold #ff0000";
          line-numbers-plus-style = "bold #00ff00";
          line-numbers-right-style = "#711c91";
          line-numbers-zero-style = "#0abdc6";
          # Not exactly standards-conformant,
          # But idk how to make it look good otherwise
          minus-emph-style = "bold syntax #880000";
          minus-style = "bold syntax #330000";
          plus-emph-style = "bold syntax #005500";
          plus-style = "bold syntax #002200";
          syntax-theme = "cyberpunk-neon";
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
}
