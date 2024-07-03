{ pkgs, ... }:
{
  home.packages = with pkgs; [
    cargo # Package manager
    rustc # Compiler
    clippy # Extra diagnostics, install globally for `cargo clippy` command
  ];

  programs.helix = {
    extraPackages = with pkgs; [
      rust-analyzer # Language server
      rustfmt # Formatter
    ];
    languages.language-server.rust-analyzer.config.check.command = "clippy";
  };
}
