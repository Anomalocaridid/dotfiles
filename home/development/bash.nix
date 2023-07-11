{ pkgs, ... }: {
  home.packages = with pkgs;[
    bats
    nodePackages.bash-language-server
    shellcheck
    shfmt
  ];
}
