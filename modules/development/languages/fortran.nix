{
  perSystem =
    { pkgs, ... }:
    {
      devshells.fortran.packages = with pkgs; [
        gnumake # Needed for exercism tests
        cmake # Needed for exercism tests
        fortls # Language server
        gfortran # Fortran compiler
      ];
    };

  unify.modules.general.home =
    { lib, pkgs, ... }:
    {
      programs.helix.languages.language = [
        {
          name = "fortran";
          auto-format = true;
          formatter.command = lib.getExe pkgs.fprettify;
        }
      ];
    };
}
