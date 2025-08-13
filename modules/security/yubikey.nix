{
  unify.modules.yubikey = {
    # Enable Yubikey support
    nixos.services.pcscd.enable = true;
    home =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ yubioath-flutter ];
      };
  };
}
