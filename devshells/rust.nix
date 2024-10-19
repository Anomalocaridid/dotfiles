{ pkgs, ... }:
{
  packages = with pkgs; [
    cargo # Package manager
    clippy # Extra diagnostics, install globally for `cargo clippy` command
    gcc # Provides linker cc
    rust-analyzer # Language server
    rustc # Compiler
    rustfmt # Formatter
  ];
}
