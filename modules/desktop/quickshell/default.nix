{ ... }:
{
  unify.modules.general = {
    home = {
      programs.quickshell = {
        enable = true;
        # systemd.enable = true;
        configs.default = ./config;
      };
    };
  };
}
