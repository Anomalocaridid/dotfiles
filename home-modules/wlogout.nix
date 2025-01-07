{
  config,
  ...
}:
{
  programs.wlogout.enable = true;

  catppuccin.wlogout.extraStyle =
    let
      convertIcon = (
        entry: icon:
        #css
        ''
          #${entry} {
            background-image: url("${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/apps/scalable/${icon}.svg");
          }
        ''
      );
    in
    ''
      ${convertIcon "lock" "system-lock-screen"}
      ${convertIcon "hibernate" "system-suspend-hibernate"}
      ${convertIcon "logout" "system-log-out"}
      ${convertIcon "shutdown" "system-shutdown"}
      ${convertIcon "suspend" "system-suspend"}
      ${convertIcon "reboot" "system-reboot"}
    '';
}
