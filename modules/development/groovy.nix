{
  perSystem =
    { pkgs, ... }:
    {
      devshells.groovy.packages = with pkgs; [
        gradle
        groovy
        jdk11 # Java development kit, default version too recent for exercism tests
      ];
    };
}
