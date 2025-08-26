{
  perSystem =
    { pkgs, ... }:
    {
      devshells.crystal.packages = with pkgs; [
        crystal
        crystalline # crystal lsp
      ];
    };

  unify.modules.development.home.programs.helix.languages.language = [
    {
      name = "crystal";
      auto-format = true;
    }
  ];
}
