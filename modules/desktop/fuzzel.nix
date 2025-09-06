{
  unify.modules.general.home =
    { config, ... }:
    {
      programs.fuzzel =
        let
          niriSettings = config.programs.niri.settings;
        in
        {
          enable = true;
          settings = {
            main = {
              dpi-aware = "no"; # appears really small otherwise
              icon-theme = config.gtk.iconTheme.name;
              terminal = "handlr launch x-scheme-handler/terminal --";
              # Match default vertical-pad
              horizontal-pad = 8;
            };
            border = {
              width = niriSettings.layout.border.width;
              # NOTE: Depends on window-rule order, chooses corner that it should match up with
              radius = builtins.ceil (builtins.elemAt niriSettings.window-rules 0)
                .geometry-corner-radius.top-right;
            };
          };
        };
    };
}
