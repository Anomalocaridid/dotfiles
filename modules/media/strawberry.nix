{
  unify.modules.strawberry.home =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        strawberry
      ];

      xdg.mimeApps.defaultApplications."audio/*" = "org.strawberrymusicplayer.strawberry.desktop";
    };
}
