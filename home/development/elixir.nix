{ pkgs, ... }:
{
  home.packages = with pkgs; [ elixir ];

  programs.helix = {
    extraPackages = with pkgs; [ elixir-ls ];
    languages.language = [
      {
        name = "elixir";
        auto-format = true;
      }
    ];
  };
}
