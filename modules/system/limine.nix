{ config, inputs, ... }:
let
  inherit (config.flake.meta) wallpaper;
in
{
  # TODO: remove when https://github.com/catppuccin/limine/pull/10 is merged
  flake-file.inputs.catppuccin-limine = {
    url = "github:catppuccin/limine/limine-config-update";
    flake = false;
  };

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
                # TODO: replace with `catppuccin.sources.limine` when https://github.com/catppuccin/limine/pull/10 is merged
                themeFile =
                  inputs.catppuccin-limine
                  + "/themes/${config.catppuccin.flavor}/catppuccin-${config.catppuccin.flavor}-${config.catppuccin.accent}.conf";

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
