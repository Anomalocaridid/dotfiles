{ moduleWithSystem, ... }:
{
  # Flake formatter
  perSystem =
    { pkgs, ... }:
    {
      formatter = pkgs.nixfmt-rfc-style;
    };

  unify.modules.general.home = moduleWithSystem (
    { self', ... }:
    { lib, pkgs, ... }:
    {
      programs.helix = {
        # nix lsp
        extraPackages = with pkgs; [ nil ];
        languages.language = [
          {
            name = "nix";
            auto-format = true;
            formatter.command = lib.getExe self'.formatter;
          }
        ];
      };
    }
  );
}
