{
  perSystem = { pkgs, ... }: {
    devshells.elixir.packages = with pkgs; [
      elixir
      elixir-ls
    ];
  };

  flake.modules.homeManager.general.programs.helix.languages.language = [
    {
      name = "elixir";
      auto-format = true;
    }
  ];
}
