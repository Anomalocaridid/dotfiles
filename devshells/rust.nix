{ pkgs, ... }:
{
  packages = with pkgs; [
    cargo # Package manager
    clippy # Extra diagnostics, install globally for `cargo clippy` command
    rust-analyzer # Language server
    rustc # Compiler
    rustfmt # Formatter
  ];
}
