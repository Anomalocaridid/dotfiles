{
  perSystem =
    { pkgs, ... }:
    {
      devshells.java.packages = with pkgs; [
        gradle # Needed for exercism tests
        jdk # Java development kit
        jdt-language-server
      ];
    };
}
