{
  unify.modules.general.home =
    { lib, ... }:
    {
      programs.cava = {
        enable = true;
        settings = {
          general.sleep_timer = 1;
          # Necessary for transparent terminal background
          color.background = lib.mkForce "default";
        };
      };
    };
}
