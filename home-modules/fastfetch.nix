{ lib, ... }:
{
  programs.fastfetch = {
    enable = true;
    settings = {
      logo.padding.top = 2;
      display.separator = " -> ";
      modules =
        let
          colorKeys = color: modules: builtins.map (module: module // { keyColor = color; }) modules;
          formatKeys =
            modules:
            let
              first = builtins.head modules;
              last = lib.lists.last modules;
            in
            builtins.map (
              module:
              module
              // {
                key = (if module == first then "" else (if module == last then "└" else "├")) + module.key;
              }
            ) modules;
          formatModules = color: modules: (formatKeys (colorKeys color modules));
        in
        lib.flatten [
          "title"
          "separator"

          (formatModules "yellow" [
            {
              type = "os";
              key = " OS";
              format = "{2}";
            }
            {
              type = "os";
              key = "";
              format = "{6}{?6} {?}{10} {8}";
            }
            {
              type = "kernel";
              key = "";
            }
            {
              type = "packages";
              key = "󰏖";
            }
            {
              type = "shell";
              key = "";
            }
          ])

          "break"

          (formatModules "blue" [
            {
              type = "wm";
              key = " DE/WM";
            }
            {
              type = "lm";
              key = "󰧨";
            }
            {
              type = "icons";
              key = "󰀻";
            }
            {
              type = "terminal";
              key = "";
            }
            {
              type = "terminalfont";
              key = "";
            }
          ])

          "break"

          (formatModules "green" [
            {
              type = "command";
              key = "󰌢 PC";
              text = "cat /sys/devices/virtual/dmi/id/board_vendor";
            }
            {
              type = "board";
              key = "";
            }
            {
              type = "cpu";
              key = "󰻠";
            }
            {
              type = "gpu";
              key = "󰍛";
            }
            {
              type = "disk";
              key = "";
            }
            {
              type = "memory";
              key = "󰑭";
            }
            {
              type = "swap";
              key = "󰓡";
            }
            {
              type = "uptime";
              key = "󰅐";
            }
            {
              type = "display";
              key = "󰍹";
            }
          ])

          "break"

          (formatModules "cyan" [
            {
              type = "sound";
              key = "  SOUND";
            }
            {
              type = "player";
              key = "󰥠";
            }
            {
              type = "media";
              key = "󰝚";
            }
          ])

          "break"
          "colors"
        ];
    };
  };
}
