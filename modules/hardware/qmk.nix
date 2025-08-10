{ inputs, ... }:
{
  flake.modules = {
    nixos.qmk.hardware.keyboard.qmk.enable = true;
    homeManager.qmk =
      { config, pkgs, ... }:
      {
        home.packages = with pkgs; [ qmk ];

        xdg.configFile."qmk/qmk.ini".source =
          (inputs.nixago.lib.${pkgs.system}.make {
            data.user.overlay_dir = "${config.home.homeDirectory}/qmk_userspace";
            output = "qmk.ini";
          }).configFile;
      };
  };
}
