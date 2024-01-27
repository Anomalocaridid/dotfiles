{ config, pkgs, ... }: {
  programs.fuzzel =
    let
      fonts = config.stylix.fonts;
      themeFile = (pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "fuzzel";
        rev = "eeb4c8d159187ef7eb59a4a99baec67c2e797e9f";
        hash = "sha256-yJvhc4ovgdxEdqFDxWNOkHJHTBF9UaCefetgCGhoG0A=";
      }) + "/themes/${config.catppuccin.flavour}.ini";
      theme =
        let
          inherit (builtins) fromJSON readFile;

          json = with pkgs;
            runCommand "converted.json" { } ''
              ${jc}/bin/jc --ini < ${themeFile} > $out;
            '';
        in
        fromJSON (readFile json);
    in
    {
      enable = true;
      settings = theme // {
        main = {
          font = "${fonts.monospace.name}:size=${toString fonts.sizes.applications}";
          dpi-aware = "no"; # appears really small otherwise
          icon-theme = config.gtk.iconTheme.name;
          terminal = "handlr launch x-scheme-handler/terminal --";
        };
      };
    };
}
