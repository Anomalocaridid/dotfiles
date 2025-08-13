{ inputs, ... }:
{
  unify.modules.qmk = {
    nixos.hardware.keyboard.qmk.enable = true;
    home =
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
