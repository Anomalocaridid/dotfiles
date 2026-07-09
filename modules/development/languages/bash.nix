{
  perSystem =
    { pkgs, ... }:
    {
      devshells.bash.packages = with pkgs; [
        bats # Needed for exercism tests
        bash-language-server
        shellcheck # More diagnostics for language server
        shfmt # Formatter
      ];
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "bash";
      auto-format = true;
      indent = {
        tab-width = 4;
        unit = "    ";
      };
    }
  ];
}
