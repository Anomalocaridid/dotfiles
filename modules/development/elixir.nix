{
  perSystem =
    { pkgs, ... }:
    {
      devshells.elixir.packages = with pkgs; [
        elixir
        elixir-ls
      ];
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "elixir";
      auto-format = true;
    }
  ];
}
