{ config, inputs, ... }:
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
            inputs.catppuccin-fuzzel + "/themes/${config.catppuccin.flavor}/${config.catppuccin.accent}.ini";
          font = "${fonts.monospace.name}:size=${toString fonts.sizes.applications}";
          dpi-aware = "no"; # appears really small otherwise
          icon-theme = config.gtk.iconTheme.name;
          terminal = "handlr launch x-scheme-handler/terminal --";
        };
      };
    };
}
