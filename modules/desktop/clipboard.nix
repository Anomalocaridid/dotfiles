{
  flake.modules.homeManager.general = { pkgs, ... }: {
    services.cliphist.enable = true;
    home.packages = with pkgs; [ wl-clipboard ];
  };
}
