{ config, ... }:
let
  inherit (config.flake.meta) wallpaper;
in
{
  unify.modules.general.nixos =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      # Interferes with setting background transparency
      catppuccin.limine.enable = false;

      boot.loader = {
        limine = {
          enable = true;
          style.wallpapers = [ wallpaper ];

          # Set background transparency
          extraConfig = builtins.readFile (
            pkgs.runCommand "catppuccin-limine"
              {
                themeFile =
                  with config.catppuccin;
                  "${sources.limine}/${flavor}/catppuccin-${flavor}-${accent}.conf";

                # 10% transparency (i.e. 90% opacity)
                transparency = "19";
              }
              ''
                cp $themeFile $out
                substituteInPlace $out \
                  --replace-fail "term_background: " "term_background: $transparency"
              ''
          );
        };

        efi.canTouchEfiVariables = true;
      };
    };
}
