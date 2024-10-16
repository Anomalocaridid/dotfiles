{ pkgs, ... }:
{
  packages = with pkgs; [
    swift
    swiftPackages.swiftpm # Needed for `swift test`
    sourcekit-lsp # Swift lsp
  ];
}
