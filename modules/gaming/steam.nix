{ config, ... }:
{

  unify.modules.desktop.nixos =
    { pkgs, ... }:
    let
      inherit (config.flake.meta) username persistDir;
    in
    {
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
}
