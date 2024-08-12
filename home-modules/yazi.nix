{
  lib,
  pkgs,
  inputs,
  ...
}:
{
  programs.yazi = {
    enable = true;
    enableFishIntegration = true;
    initLua = ./.config/yazi/init.lua;
    plugins =
      (lib.attrsets.mapAttrs' (
        x: _: lib.attrsets.nameValuePair (lib.strings.removeSuffix ".yazi" x) (./.config/yazi/plugins/${x})
      ) (builtins.readDir ./.config/yazi/plugins))
      // (with inputs; {
        # Previewers
        "glow" = glow-yazi;
        "miller" = miller-yazi;
        "exifaudio" = exifaudio-yazi;
        "ouch" = ouch-yazi;
        # Functional Plugins
        ## Jumping
        "relative-motions" = relative-motions-yazi;
        ## filter enhancements
        "smart-filter" = yazi-plugins + /smart-filter.yazi;
        "starship" = starship-yazi;
        # UI enhancements
        "full-border" = yazi-plugins + /full-border.yazi;
      });
    settings = {
      plugin = {
        prepend_previewers =
          [
            {
              name = "*.md";
              run = "glow";
            }
            {
              mime = "text/csv";
              run = "miller";
            }
            {
              mime = "audio/*";
              run = "exifaudio";
            }
          ]
          ++ (builtins.map
            (type: {
              mime = "application/${type}";
              run = "ouch";
            })
            [
              "*zip"
              "x-tar"
              "x-bzip2"
              "x-7z-compressed"
              "x-rar"
              "x-xz"
            ]
          );
      };
      opener.open = [
        {
          run = ''xdg-open "$1"'';
          desc = "Open";
          orphan = true; # Ensure it stays open after yazi is closed
        }
      ];
    };
    keymap = {
      input.prepend_keymap = [
        {
          on = [ "<Esc>" ];
          run = "close";
          desc = "Cancel input";
        }
      ];
      manager.prepend_keymap =
        [
          {
            on = [ "l" ];
            run = "plugin --sync smart-enter";
            desc = "Enter the child directory, or open the file";
          }
          {
            on = [ "p" ];
            run = "plugin --sync smart-paste";
            desc = "Paste into the hovered directory or CWD";
          }
          {
            on = [ "<C-n>" ];
            run = ''shell '${lib.getExe pkgs.xdragon} --and-exit --all --on-top "$@"' --confirm'';
            desc = "Drag and drop selected files with dragon";
          }
          {
            on = [ "k" ];
            run = "plugin --sync arrow --args=-1";
            desc = "Move cursor up (wrapping)";
          }
          {
            on = [ "j" ];
            run = "plugin --sync arrow --args=1";
            desc = "Move cursor down (wrapping)";
          }
          {
            on = [ "f" ];
            run = "plugin smart-filter";
            desc = "Smart filter";
          }
        ]
        ++ (builtins.genList (
          x:
          let
            i = toString (x + 1);
          in
          {
            on = [ "${i}" ];
            run = "plugin relative-motions --args=${i}";
            desc = "Move in relative steps";
          }
        ) 9);
    };
  };

  # Dependencies for plugins
  home.packages = with pkgs; [
    # Previewers
    glow
    miller
    exiftool
    ouch
  ];
}
