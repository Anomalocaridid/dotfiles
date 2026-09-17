{
  perSystem = { pkgs, ... }: {
    devshells.standard-ml.packages = with pkgs; [
      polyml # Standard ML compiler
    ];
  };

  flake.modules.homeManager.general = { lib, pkgs, ... }: {
    programs.helix.languages.language = [
      {
        name = "sml";
        auto-format = true;
        formatter.command = lib.getExe pkgs.smlfmt;
        indent = {
          tab-width = 2;
          unit = "  ";
        };
      }
    ];
  };
}
