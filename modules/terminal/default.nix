{
  unify.modules.general.home =
    { lib, pkgs, ... }:
    {
      home.shellAliases = {
        rd = "rmdir";
        md = "mkdir";
        rm = "rm --interactive";
        du = lib.getExe pkgs.du-dust;
        df = lib.getExe pkgs.duf;
      };

      programs = {
        ripgrep.enable = true;
        fd.enable = true;
      };
    };
}
