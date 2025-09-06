{
  perSystem =
    { pkgs, ... }:
    {
      devshells.kotlin.packages = with pkgs; [
        gradle # Needed for exercism tests
        jdk # Java development kit
        kotlin-language-server
      ];
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "kotlin";
      auto-format = true;
    }
  ];
}
