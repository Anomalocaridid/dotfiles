{ pkgs, ... }:
{
  packages = with pkgs; [
    ruby
    rubyPackages.minitest # needed for exercism tests
    rubyPackages.solargraph # ruby language server
  ];
}
