{
  perSystem = { pkgs, ... }: {
    devshells.java.packages = with pkgs; [
      gradle # Needed for exercism tests
      jdk # Java development kit
      jdt-language-server
    ];
  };

  flake.modules.homeManager.general.programs.helix.languages.language = [
    {
      name = "java";
      auto-format = true;
      indent = {
        tab-width = 4;
        unit = "    ";
      };
    }
  ];
}
