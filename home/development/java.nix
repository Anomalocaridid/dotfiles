{ pkgs, ... }: {
  home.packages = with pkgs; [
    gradle
    jdk
    jdt-language-server
  ];
}
