{
  perSystem =
    { pkgs, ... }:
    {
      devshells.crystal.packages = with pkgs; [
        crystal
        crystalline # crystal lsp
      ];
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "crystal";
      auto-format = true;
    }
  ];
}
