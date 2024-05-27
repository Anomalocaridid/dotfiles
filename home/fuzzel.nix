{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.fuzzel =
    let
      fonts = config.stylix.fonts;
      themeFile = pkgs.sources.catppuccin-fuzzel + "/themes/${config.catppuccin.flavor}.ini";
      theme =
        let
          inherit (builtins) fromJSON readFile;

          json = pkgs.runCommand "converted.json" { } ''
            ${lib.getExe pkgs.jc} --ini < ${themeFile} > $out;
          '';
        in
        fromJSON (readFile json);
    in
    {
      enable = true;
      settings = theme // {
        main = {
          font = "${fonts.monospace.name}:size=${toString fonts.sizes.applications}";
          dpi-aware = "no"; # appears really small otherwise
          icon-theme = config.gtk.iconTheme.name;
          terminal = "handlr launch x-scheme-handler/terminal --";
        };
      };
    };
}
