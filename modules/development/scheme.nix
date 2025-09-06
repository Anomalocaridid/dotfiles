{
  perSystem =
    { pkgs, ... }:
    {
      devshells.scheme.packages = with pkgs; [
        guile
        gnumake # Needed for exercism tests
      ];
    };

  unify.modules.general.home =
    { lib, pkgs, ... }:
    {
      programs.helix.languages = {
        language-server.scheme-langserver.command = lib.getExe' pkgs.akkuPackages.scheme-langserver "scheme-langserver";
        language = [
          {
            name = "scheme";
            auto-format = true;
            language-servers = [ "scheme-langserver" ];
          }
        ];
      };
    };
}
