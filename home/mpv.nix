{ config, lib, pkgs, ... }: {
  programs.mpv =
    let
      fonts = config.stylix.fonts;
      palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
    in
    {
      enable = true;
      config = {
        osd-font = fonts.sansSerif.name;
        osd-back-color = "#${palette.overlay0.hex}";
        osd-border-color = "#${palette.mantle.hex}";
        osd-color = "#${palette.text.hex}";
        osd-fractions = true;
        osd-shadow-color = "#${palette.base.hex}";
        background = "#${palette.base.hex}";
        volume = 40;
      };

      profiles = {
        eye-cancer = {
          sharpen = 5;
          osd-font = "Comic Sans MS";
        };
      };

      scriptOpts = {
        console.font = fonts.monospace.name;
        osc.seekbarstyle = "diamond";
        stats =
          let
            # Need BBGGRR format
            rgb2Bgr = rgb: lib.trivial.pipe rgb
              [
                # Split into 3 chunks of 2 (i.e. RR GG BB)
                (x: builtins.genList (n: builtins.substring (n * 2) 2 x) 3)
                lib.lists.reverseList
                lib.strings.concatStrings
              ];
          in
          {
            font = fonts.monospace.name;
            font_mono = fonts.monospace.name;
            font_color = rgb2Bgr palette.text.hex;
            border_color = rgb2Bgr palette.mantle.hex;
            plot_bg_border_color = rgb2Bgr palette.${config.catppuccin.accent}.hex;
            plot_bg_color = rgb2Bgr palette.mantle.hex;
            plot_color = rgb2Bgr palette.${config.catppuccin.accent}.hex;
          };
      };
    };
}
