{ config, ... }:
{
  programs.fuzzel =
    let
      fonts = config.stylix.fonts;
    in
    {
      enable = true;
      settings = {
        main = {
          font = "${fonts.monospace.name}:size=${toString fonts.sizes.applications}";
          dpi-aware = "no"; # appears really small otherwise
          icon-theme = config.gtk.iconTheme.name;
          terminal = "handlr launch x-scheme-handler/terminal --";
        };
      };
    };
}
