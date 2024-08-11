{ lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    nixpkgs-review # Used to check rebuilds caused by changes to nixpkgs
  ];

  programs.helix = {
    extraPackages = with pkgs; [
      nil # lsp
    ];
    languages.language = [
      {
        name = "nix";
        auto-format = true;
        formatter.command = lib.getExe pkgs.nixfmt-rfc-style;
      }
    ];
  };
}
