{
  perSystem =
    { pkgs, ... }:
    {
      devshells.zig.packages = with pkgs; [
        zig
        zls # Zig language server
      ];
    };
}
