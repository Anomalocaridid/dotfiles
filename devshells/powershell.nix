{ pkgs, ... }:
{
  packages = with pkgs; [ powershell ];

  # Install test runner, needed for exercism tests
  # It takes a while, so check if it needs to be installed first
  devshell.startup."Install Pester".text = ''
    pwsh -Command "if (-Not (Get-Module -ListAvailable -Name Pester)) { Install-Module -Name Pester -Force -SkipPublisherCheck }"
  '';
}
