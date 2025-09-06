{ config, inputs, ... }:
{
  unify.modules.general = {
    nixos =
      let
        inherit (config.flake.meta) username persistDir;
      in
      {
        hardware.keyboard.qmk.enable = true;

        environment.persistence.${persistDir}.users.${username}.directories = [
          "qmk_firmware" # QMK firmware
          "qmk_userspace" # QMK userspace
        ];
      };

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
