{
  perSystem =
    { pkgs, ... }:
    {
      devshells.crystal.packages = with pkgs; [
        crystal
        crystalline # crystal lsp
      ];
    };
}
