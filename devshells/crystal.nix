{ pkgs, ... }:
{
  packages = with pkgs; [
    crystal
    crystalline # crystal lsp
  ];
}
