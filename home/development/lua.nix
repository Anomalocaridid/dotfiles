{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (lua5_4.withPackages (
      ps: with ps; [
        busted # Needed for exercism tests
      ]
    ))
  ];

  programs.helix = {
    extraPackages = with pkgs; [ lua-language-server ];

    languages.language = [
      {
        name = "lua";
        auto-format = true;
      }
    ];
  };
}
