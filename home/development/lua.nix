{ pkgs, ... }: {
  home.packages = with pkgs; [
    (lua5_4.withPackages
      (ps: with ps;
      [
        busted
      ]))
    lua-language-server
  ];
}
