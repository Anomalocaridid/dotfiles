{ lib, pkgs, ... }:
{
  home.packages = with pkgs; [ nim ];

  programs.helix.languages = {
    # TODO: replace with default lsp when added to nixpkgs
    language-server.nimlsp.command = lib.getExe' pkgs.nimlsp "nimlsp";
    language = [
      {
        name = "nim";
        auto-format = true;
        language-servers = [ "nimlsp" ];
        formatter.command = "nimpretty"; # Included with nim
      }
    ];
  };
}
