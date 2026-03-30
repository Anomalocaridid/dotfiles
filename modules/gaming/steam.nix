{ config, ... }:
{
  unify.modules.desktop = {
    nixos =
      { pkgs, ... }:
      let
        inherit (config.flake.meta) username persistDir;
      in
      {
        nixpkgs.config.allowUnfreePackages = [
          "steam"
          "steam-unwrapped"
        ];

        programs.steam = {
          enable = true;
          remotePlay.openFirewall = true;
          dedicatedServer.openFirewall = true;
          # Add extra compatibility tools to Steam
          extraCompatPackages = with pkgs; [ proton-ge-bin ];
        };

        environment.persistence.${persistDir}.users.${username}.directories = [
          ".config/unity3d" # Needed for some games' settings
          ".local/share/Steam" # Steam games and save data
          ".local/share/Tabletop Simulator" # Tabletop Simulator settings
        ];
      };
    home =
      {
        lib,
        pkgs,
        osConfig,
        ...
      }:
      {
        xdg.autostart.entries = lib.singleton (
          pkgs.makeDesktopItem {
            name = "steam-autostart";
            desktopName = "Steam (Autostart)";
            exec = "${lib.getExe osConfig.programs.steam.package} -silent";
            # Make it more concise to get path to desktop file
            destination = "/";
          }
          + "/steam-autostart.desktop"
        );
      };
  };
}
