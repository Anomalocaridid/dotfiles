{ lib, ... }:
{
  programs.cava = {
    # TODO: Re-enable when nixpkgs#355948 makes it to master
    # enable = true;
    settings = {
      general.sleep_timer = 1;
      # Necessary for transparent terminal background
      color.background = lib.mkForce "default";
    };
  };
}
