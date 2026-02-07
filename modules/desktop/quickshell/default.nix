{ ... }:
{
  unify.modules.general.home =
    { pkgs, ... }:
    {
      programs.quickshell = {
        enable = true;
        # systemd.enable = true;
        configs.default = ./config;
      };
    };
}
