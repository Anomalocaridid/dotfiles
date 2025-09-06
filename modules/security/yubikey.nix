{ config, ... }:
{
  unify.modules.general = {
    nixos =
      let
        inherit (config.flake.meta) persistDir username;
      in
      {
        # Enable Yubikey support
        services.pcscd.enable = true;

        environment.persistence.${persistDir}.users.${username}.directories = [
          ".local/share/com.yubico.authenticator" # Yubico auth settings (may have secrets?)
        ];
      };

    home =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ yubioath-flutter ];
      };
  };
}
