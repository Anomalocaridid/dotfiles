{
  perSystem = { pkgs, ... }: {
    devshells.php.packages = with pkgs; [
      php
      phpunit # Unit testing framework
    ];
  };

  flake.modules.homeManager.general = { lib, pkgs, ... }: {
    programs.helix.languages = {
      language-server.phpactor = {
        command = lib.getExe pkgs.phpactor;
        args = [ "language-server" ];
      };
      language = [
        {
          name = "php";
          auto-format = true;
          language-servers = [ "phpactor" ];
        }
      ];
    };
  };
}
