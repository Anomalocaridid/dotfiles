{ pkgs, ... }:
{
  packages = with pkgs; [
    php
    phpunit # Unit testing framework
  ];
}
