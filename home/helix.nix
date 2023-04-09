{ config, ... }:
{
  programs.helix = {
    enable = true;
    settings = {
      editor = {
        bufferline = "multiple";
        lsp.display-messages = true;
        indent-guides.render = true;
        statusline = {
          left = [
            "mode"
            "separator"
            "spinner"
            "file-name"
            "separator"
          ];
          # center = [];
          right = [
            "diagnostics"
            "separator"
            "file-type"
            "separator"
            "selections"
            "separator"
            "position"
            "file-encoding"
          ];
          separator = "";
        };
      };
    };
    languages = [
      {
        name = "lua";
        auto-format = true;
      }
      { 
        name = "haskell";
        auto-format = true;
      }
      {
        name = "python";
        auto-format = true;
      }
      {
        name = "bash";
        indent = {
          tab-width = 4;
          unit = "\t";
        };
        formatter = { command = "shfmt"; };
        auto-format = true;
      }
      {
        name = "c";
        indent = {
          tab-width = 4;
          unit = " ";
        };
        auto-format = true;
      }
      {
        name = "julia";
        auto-format = true;
      }
    ];
  };
}
