{
  perSystem = { pkgs, ... }: {
    devshells.crystal.packages = with pkgs; [
      crystal
      crystalline # crystal lsp
    ];
  };

  flake.module.homeManager.general.programs.helix.languages.language = [
    {
      name = "crystal";
      auto-format = true;
    }
  ];
}
