{ inputs, ... }:
{
  unify.modules.ly.nixos =
    { pkgs, ... }:
    {
      services.displayManager.ly = {
        enable = true;
        settings = {
          animation = "doom";
          blank_password = true;
          hide_borders = true;
        };
      };

      # Set default session
      environment.etc."ly/save.ini".source =
        (inputs.nixago.lib.${pkgs.system}.make {
          output = "save.ini";
          data.globalSection = {
            user = "anomalocaris";
            session_index = 2;
          };
          format = "iniWithGlobalSection";
        }).configFile;

      # Ensure services start properly
      systemd.services.display-manager.environment.XDG_CURRENT_DESKTOP = "X-NIXOS-SYSTEMD-AWARE";
    };
}
