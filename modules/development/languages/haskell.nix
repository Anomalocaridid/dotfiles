{
  perSystem = { pkgs, ... }: {
    devshells.haskell.packages = with pkgs; [
      cabal-install # Package manager
      ghc # Compiler
      haskell-language-server
      hlint # Linter
      ormolu # Formatter
      stack # Package manager
    ];
  };

  flake.modules.homeManager.general = {
    home.file.".ghci".text = ''
      :set prompt "\ESC[1;35mλ> \ESC[m"
    '';

    programs.helix.languages.language = [
      {
        name = "haskell";
        auto-format = true;
      }
    ];
  };
}
