{
  unify.modules.general.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      programs = {
        fd.enable = true;
        ripgrep.enable = true;
        vivid = {
          enable = true;
          # TODO: remove when catppuccin/nix vivid module is added
          activeTheme = "catppuccin-${config.catppuccin.flavor}";
        };
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
