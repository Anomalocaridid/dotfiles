{ pkgs, ... }: {
  home.packages = with pkgs; [
    nim
    nimlsp
  ];
}
