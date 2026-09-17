{
  perSystem = { pkgs, ... }: {
    devshells.perl.packages = with pkgs; [
      perl
      perl540Packages.PerlCritic
      perl540Packages.PerlTidy
      perlnavigator # Language server
    ];
  };

  flake.modules.homeManager.general.programs.helix.languages.language = [
    {
      name = "perl";
      auto-format = true;
    }
  ];
}
