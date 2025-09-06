{
  perSystem =
    { pkgs, ... }:
    {
      devshells.unison.packages = with pkgs; [ unison-ucm ];
    };

  unify.modules.general.home =
    { lib, pkgs, ... }:
    {
      programs.helix.languages = {
        language-server.unison-language-server = {
          command = lib.getExe pkgs.netcat;
          args = [
            "localhost"
            "5757"
          ];
        };
        languages = [
          {
            name = "unison";
            auto-format = true;
            language-servers = [ "unison-language-server" ];
          }
        ];
      };
    };
}
