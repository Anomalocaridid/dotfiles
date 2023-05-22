{ lib, pkgs, ... }: {
  # Qt theme defined in configuration.nix because it works better at system level
  gtk =
    let
      extraConfig = {
        # Use string, not bool because toString converts bool to "1", not "true"
        gtk-application-prefer-dark-theme = "true";
      };
      inherit (lib.strings) concatStringsSep;
      inherit (lib.attrsets) mapAttrsToList;
      gtk2ExtraConfig = concatStringsSep "\n"
        (mapAttrsToList (k: v: "${k} = ${builtins.toString v}")
          extraConfig);
    in
    {
      enable = true;
      theme = {
        name = "materia-cyberpunk-neon";
        package = pkgs.callPackage ../pkgs/gtk-materia-cyberpunk-neon { };
      };
      iconTheme = {
        name = "Sweet-Rainbow";
        package = pkgs.callPackage ../pkgs/candy-icons { };
      };
      gtk2.extraConfig = gtk2ExtraConfig;
      gtk3.extraConfig = extraConfig;
      gtk4.extraConfig = extraConfig;
    };

  home.pointerCursor = {
    name = "Breeze_Hacked";
    package = pkgs.callPackage ../pkgs/breeze-hacked-cursor { };
    gtk.enable = true;
  };
}
