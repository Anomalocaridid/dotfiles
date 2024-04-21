{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [
      cabal-install # Package manager
      ghc # Compiler
      stack # Package manager
    ];
    file.".ghci".text = ''
      :set prompt "\ESC[1;35mλ> \ESC[m"
    '';
  };

  programs.helix = {
    extraPackages = with pkgs; [
      haskell-language-server
      hlint # Linter
      ormolu # Formatter
    ];

    languages.language = [
      {
        name = "haskell";
        auto-format = true;
      }
    ];
  };
}
