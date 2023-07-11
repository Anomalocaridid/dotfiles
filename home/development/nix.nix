{ pkgs, ... }: {
  home.packages = with pkgs;[
    nil # lsp
    nixpkgs-fmt
    nixpkgs-review # Used to check rebuilds caused by changes to nixpkgs
  ];
}
