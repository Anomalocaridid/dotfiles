{ pkgs, ... }:
{
  xdg = {
    mimeApps = {
      enable = true;
      defaultApplications =
        let
          editor = "Helix.desktop";
          browser = "firefox.desktop";
          pdfViewer = "org.pwmt.zathura-pdf-mupdf.desktop";
          wordProcessor = "writer.desktop";
          spreadsheet = "calc.desktop";
          presentation = "impress.desktop";
        in
        {
          "application/epub+zip" = pdfViewer;
          "application/msword" = wordProcessor;
          "application/oxps" = pdfViewer;
          "application/pdf" = pdfViewer;
          "application/toml" = editor;
          "application/vnd.ms-excel" = spreadsheet;
          "application/vnd.ms-powerpoint" = presentation;
          "application/vnd.oasis.opendocument.database" = "base.desktop";
          "application/vnd.oasis.opendocument.formula" = "math.desktop";
          "application/vnd.oasis.opendocument.graphics" = "draw.desktop";
          "application/vnd.oasis.opendocument.presentation" = presentation;
          "application/vnd.oasis.opendocument.presentation-template" = presentation;
          "application/vnd.oasis.opendocument.spreadsheet" = spreadsheet;
          "application/vnd.oasis.opendocument.spreadsheet-flat-xml" = spreadsheet;
          "application/vnd.oasis.opendocument.text" = wordProcessor;
          "application/vnd.oasis.opendocument.text-template" = wordProcessor;
          "application/vnd.openxmlformats-officedocument.presentationml.presentation" = presentation;
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = spreadsheet;
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = wordProcessor;
          "application/vnd.openxmlformats-officedocument.wordprocessingml.template" = wordProcessor;
          "application/x-yaml" = editor;
          "audio/*" = "org.strawberrymusicplayer.strawberry.desktop";
          "image/*" = "imv.desktop";
          "inode/directory" = "yazi.desktop";
          "text/*" = editor;
          "text/html" = browser;
          "video/*" = "mpv.desktop";
          "x-scheme-handler/http" = browser;
          "x-scheme-handler/https" = browser;
          "x-scheme-handler/terminal" = "org.wezfurlong.wezterm.desktop";
        };
    };

    configFile."handlr/handlr.toml".source =
      let
        tomlFormat = pkgs.formats.toml { };
      in
      tomlFormat.generate "handlr-config" {
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
  };

  home.packages = with pkgs; [
    handlr-regex
    # Use handlr as drop-in replacement for xdg-open
    (writeShellApplication {
      name = "xdg-open";
      runtimeInputs = [ handlr-regex ];
      text = # shell
        ''
          handlr open "$@"
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
