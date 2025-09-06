{
  unify.modules.general.home =
    { pkgs, ... }:
    {
      # InfiniTime watch manager
      home.packages = with pkgs; [ itd ];
    };
}
