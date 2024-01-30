{ pkgs, ... }: {
  home.packages = with pkgs; [
    cargo # Package manager
    rustc # Compiler
  ];

  programs.helix.extraPackages = with pkgs;[
    rust-analyzer # Language server
    rustfmt # Formatter
  ];
}
