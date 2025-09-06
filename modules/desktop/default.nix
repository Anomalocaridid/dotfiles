{
  unify.modules.general.home =
    { pkgs, ... }:
    {
      # Calculator program
      home.packages = with pkgs; [ qalculate-gtk ];
    };
}
