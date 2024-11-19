{ pkgs, inputs, ... }:
{
  xdg = {
    mimeApps = {
      enable = true;
      defaultApplications =
        let
          editor = "Helix.desktop";
          browser = "librewolf.desktop";
          pdfViewer = "org.pwmt.zathura-pdf-mupdf.desktop";
          officeSuite = "startcenter.desktop";
        in
        {
          "application/epub+zip" = pdfViewer;
          "application/msword" = "writer.desktop";
          "application/oxps" = pdfViewer;
          "application/pdf" = pdfViewer;
          "application/toml" = editor;
          "application/vnd.ms-excel" = "calc.desktop";
          "application/vnd.ms-powerpoint" = "impress.desktop";
          "application/vnd.oasis.opendocument.*" = officeSuite;
          "application/vnd.openxmlformats-officedocument.*" = officeSuite;
          "application/x-yaml" = editor;
          "audio/*" = "org.strawberrymusicplayer.strawberry.desktop";
          "image/*" = "imv.desktop";
          "inode/directory" = "yazi.desktop";
          "text/*" = editor;
          "text/html" = browser;
          "video/*" = "mpv.desktop";
          "x-scheme-handler/http*" = browser;
          "x-scheme-handler/terminal" = "org.wezfurlong.wezterm.desktop";
        };
    };

    configFile."handlr/handlr.toml".source =
      (inputs.nixago.lib.${pkgs.system}.make {
        data = {
          enable_selector = false;
          selector = "fuzzel --dmenu --prompt='Open With: '";
          term_exec_args = "";
          handlers = [
            {
              exec = "freetube %u";
              regexes = [ "youtu(be.com|.be)" ];
            }
            {
              exec = "handlr open steam://openurl/%u";
              regexes = [ "^https://([[:alpha:]]*\.)?steam(powered|community).com/" ];
            }
          ];
        };
        output = "handlr.toml";
      }).configFile;
  };

  home.packages = with pkgs; [
    handlr-regex
    # Use handlr as drop-in replacement for xdg-open
    (writeShellApplication {
      name = "xdg-open";
      runtimeInputs = [ handlr-regex ];
      text = # shell
        ''
          handlr open -- "$@"
        '';
    })
    # Use handlr as drop-in replacement for xterm
    (writeShellApplication {
      name = "xterm";
      runtimeInputs = [ handlr-regex ];
      text = # shell
        ''
          handlr launch x-scheme-handler/terminal -- "$@"
        '';
    })
  ];
}
