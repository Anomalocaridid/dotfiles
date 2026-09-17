{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  flake.modules = {
    nixos.general = {
      # Enable Yubikey support
      services.pcscd.enable = true;

      environment.persistence.${persistDir}.users.${username}.directories = [
        ".local/share/com.yubico.authenticator" # Yubico auth settings (may have secrets?)
      ];
    };

    homeManager.general = { pkgs, ... }: {
      home.packages = with pkgs; [ yubioath-flutter ];
    };
  };
}
