{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      cabal-install # package manager
      ghc
      haskell-language-server
      hlint
      ormolu # formatter
      stack # package manager
    ];
    file.".ghci".text = ''
      :set prompt "\ESC[1;35mλ> \ESC[m"
    '';
  };
}
