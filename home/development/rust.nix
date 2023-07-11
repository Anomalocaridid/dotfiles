{ pkgs, ... }: {
  home.packages = with pkgs; [
    cargo
    rustc
    rust-analyzer
  ];
}
