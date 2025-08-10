{
  flake.modules = {
    # Enable Yubikey support
    nixos.yubikey.services.pcscd.enable = true;
    homeManager.yubikey =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ yubioath-flutter ];
      };
  };
}
