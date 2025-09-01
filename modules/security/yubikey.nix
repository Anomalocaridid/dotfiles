{ config, ... }:
{
  unify.modules.yubikey = {
    # Enable Yubikey support
    nixos =
      let
        inherit (config.flake.meta) persistDir username;
      in
      {
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
