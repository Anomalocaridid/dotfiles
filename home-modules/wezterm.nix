{ config, lib, ... }:
{
  programs.wezterm = {
    enable = true;
    extraConfig =
      let
        fonts = config.stylix.fonts;
        luaFormat = lib.generators.toLua { };
        lua = luaFormat {
          hide_tab_bar_if_only_one_tab = true;
          window_background_opacity = 0.9;
          exit_behavior = "Close";
          check_for_updates = false;
          # enable_wayland = true;
          # Re-enable once nixpkgs#336069 is fixed
          enable_wayland = false;
          enable_kitty_graphics = true;
          window_close_confirmation = "NeverPrompt";
          # Remove when nixpkgs#336069 is fixed
          front_end = "WebGpu";

          font_size = fonts.sizes.terminal;
          font =
            lib.generators.mkLuaInline # lua
              ''
                wezterm.font${
                  luaFormat {
                    family = fonts.monospace.name;
                    harfbuzz_features = [
                      "ss09" # >>= <<= ||= |=
                      "cv25" # .-
                      "cv26" # :-
                      "cv32" # .=
                      "cv27" # []
                      "cv28" # {. .}
                      "ss06" # \\
                      "ss07" # =~ !~
                    ];
                  }
                }
              '';
          color_scheme = "catppuccin-${config.catppuccin.flavor}";
        };
      in
      "return ${lua}";
  };
}
