{
  perSystem = { pkgs, ... }: {
    devshells.nim.packages = with pkgs; [
      nim
      nimlsp
    ];
  };

  flake.modules.homeManager.general = { lib, pkgs, ... }: {
    programs.helix.languages.language = [
      {
        name = "nim";
        auto-format = true;
        formatter.command = lib.getExe' pkgs.nim "nimpretty";
        language-servers = [ "nimlsp" ];
      }
    ];
  };
}
