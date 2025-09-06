{
  perSystem =
    { pkgs, ... }:
    {
      devshells.perl.packages = with pkgs; [
        perl
        perl540Packages.PerlCritic
        perl540Packages.PerlTidy
        perlnavigator # Language server
      ];
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "perl";
      auto-format = true;
    }
  ];
}
