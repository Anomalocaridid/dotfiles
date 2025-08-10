{
  flake.modules.homeManager.gimp =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [ gimp3-with-plugins ];
      xdg.configFile."GIMP/3.0/gimprc".text = # scheme
        ''
          (theme "System")
        '';
    };
}
