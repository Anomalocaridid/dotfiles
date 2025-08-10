{
  perSystem =
    { pkgs, ... }:
    {
      devshells.php.packages = with pkgs; [
        php
        phpunit # Unit testing framework
      ];
    };
}
