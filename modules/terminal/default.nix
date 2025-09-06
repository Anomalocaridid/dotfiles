{
  unify.modules.general.home =
    { lib, pkgs, ... }:
    {
      programs = {
        ripgrep.enable = true;
        fd.enable = true;
      };

      home = {
        shellAliases = {
          rd = "rmdir";
          md = "mkdir";
          rm = "rm --interactive";
          du = lib.getExe pkgs.du-dust;
          df = lib.getExe pkgs.duf;
        };

        packages = with pkgs; [
          killall
          tree
        ];
      };
    };
}
