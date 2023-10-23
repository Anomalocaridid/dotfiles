{ pkgs, ... }: {
  home.packages = with pkgs; [
    gradle
    jdk
    java-language-server
    google-java-format
  ];
}
