{ pkgs, ... }: {
  programs.nnn = {
    enable = true;
    bookmarks = {
      d = "~/Documents";
      D = "~/Downloads";
      e = "~/exercism";
      n = "~/nixos";
      p = "~/Pictures";
      v = "~/Videos";
    };
    plugins = {
      mappings = {
        c = "chksum";
        d = "dragdrop";
        M = "nmount";
        n = "bulknew";
        s = "!zsh -i";
        z = "autojump";
      };
      src = "${pkgs.nnn}/share/plugins";
    };
    extraPackages =
      with pkgs;
      let
        wrapAdvcpmv = (util:
          writeShellApplication {
            name = util + "g";
            runtimeInputs = [ custom.advcpmv-coreutils ];
            text = ''
              ${util} "$@"
            '';
          }
        );
      in
      [
        pmount
        udisks
        xdragon
        custom.advcpmv-coreutils # add progress bars to cp and mv (depends on overlay in flake.nix)
      ] ++ (map wrapAdvcpmv [ "cp" "mv" ]); # nnn support for advcpmv-coreutils
  };

  home.sessionVariables = {
    NNN_OPTS = "cDEirx";
    NNN_OPENER = "$HOME/.config/nnn/plugins/nuke";
    GUI = 1;
    NNN_FIFO = "/tmp/nnn.fifo";
    NNN_COLORS = "#1909c9d02e0d2cff";
  };
}
