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
      boot.loader = {
        grub = {
          device = "nodev";
          efiSupport = true;
          # Change GRUB theme wallpaper
          splashImage = lib.mkForce wallpaper;
          theme = lib.mkForce (
            pkgs.runCommand "catppuccin-grub-background" { } ''
              cp --recursive --no-preserve=mode ${config.catppuccin.sources.grub}/share/grub/themes/catppuccin-${config.catppuccin.flavor}-grub-theme/ "$out"

              # Replace background
              rm "$out"/background.png
              cp ${wallpaper} "$out"/background.png
            ''
          );
        };
        efi.canTouchEfiVariables = true;
      };
    };
}
