{
  flake.modules.homeManager.general = { pkgs, ... }: {
    # InfiniTime watch manager
    home.packages = with pkgs; [ itd ];
  };
}
