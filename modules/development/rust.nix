{
  perSystem =
    { pkgs, ... }:
    {
      devshells.rust.packages = with pkgs; [
        cargo # Package manager
        clippy # Extra diagnostics, install globally for `cargo clippy` command
        gcc # Provides linker cc
        rust-analyzer # Language server
        rustc # Compiler
        rustfmt # Formatter
      ];
    };

  unify.modules.general.home.programs.helix.languages.language-server.rust-analyzer.config.check.command =
    "clippy";
}
