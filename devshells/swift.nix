{ pkgs, ... }:
{
  packages = with pkgs; [
    sourcekit-lsp # Swift lsp
    swift
    swift-format # Code formatter
    swiftPackages.swiftpm # Needed for `swift test`
  ];
}
