{
  flake.modules.homeManager.general = { pkgs, ... }: {
    programs.numbat = {
      enable = true;
      settings.exchange-rates.fetching-policy = "on-first-use";
    };
  };
}
