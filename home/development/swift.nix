{ pkgs, ... }:
{
  home.packages = with pkgs; [
    swift
    swiftPackages.swiftpm # Needed for `swift test`
  ];

  programs.helix.extraPackages = with pkgs; [ sourcekit-lsp ];
}
