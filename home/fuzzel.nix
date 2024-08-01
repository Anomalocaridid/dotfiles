{ config, pkgs, ... }:
{
  programs.fuzzel =
    let
      fonts = config.stylix.fonts;
    in
    {
      enable = true;
      settings = {
        main = {
          include =
            pkgs.sources.catppuccin-fuzzel
            + "/themes/${config.catppuccin.flavor}/${config.catppuccin.accent}.ini";
          font = "${fonts.monospace.name}:size=${toString fonts.sizes.applications}";
          dpi-aware = "no"; # appears really small otherwise
          icon-theme = config.gtk.iconTheme.name;
          terminal = "handlr launch x-scheme-handler/terminal --";
        };
      };
    };
}
