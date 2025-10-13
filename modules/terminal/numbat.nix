{
  unify.modules.general.home =
    { pkgs, ... }:
    {
      programs.numbat = {
        enable = true;
        settings.exchange-rates.fetching-policy = "on-first-use";
      };
    };
}
