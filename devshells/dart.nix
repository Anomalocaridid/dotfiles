{ pkgs, ... }:
{
  packages = with pkgs; [
    dart
  ];

  # Make sure telemetry is disabled. Thanks Google :/
  devshell.startup."disable-analytics".text = ''
    dart --disable-analytics
  '';
}
