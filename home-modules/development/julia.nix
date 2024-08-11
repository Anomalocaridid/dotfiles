{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (julia.withPackages [
      # Don't separate LanguageServer into helix's extraPackages
      # because that would require compiling Julia's depot twice
      # and this is not a separate program anyways
      "LanguageServer"
    ])
  ];

  programs.helix.languages.language = [
    {
      name = "julia";
      auto-format = true;
    }
  ];
}
