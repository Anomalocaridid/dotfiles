{
  perSystem =
    { pkgs, ... }:
    {
      devshells.raku.packages = with pkgs; [
        perl540Packages.PerlCritic
        perl540Packages.PerlTidy
        perlnavigator # Language server
        rakudo
      ];
    };
}
