{ lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    gradle # Needed for exercism tests
    jdk # Java development kit
  ];

  programs.helix = {
    languages = {
      language-server.java-language-server.command = lib.getExe pkgs.java-language-server;
      language = [
        {
          name = "java";
          auto-format = true;
          indent = {
            tab-width = 4;
            unit = "    ";
          };
          # TODO: replace with jdtls (default) if it gets fixed
          language-servers = [ "java-language-server" ];
          formatter.command = lib.getExe pkgs.google-java-format;
        }
      ];
    };
  };
}
