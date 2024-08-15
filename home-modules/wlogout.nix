{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.wlogout = {
    enable = true;
    style =
      let
        # TODO: find a way to use svg icons with wlogout without converting them to png
        convertIcon = (
          entry: icon:
          let
            iconFile = "${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/apps/scalable/${icon}.svg";
            # NOTE: Ensure icons are high enough quality to not look fuzzy
            png = pkgs.runCommand "${icon}.png" { } ''
              ${lib.getExe pkgs.inkscape} ${iconFile} --export-width=1024 --export-height=1024 --export-filename=$out
            '';
          in
          #css
          ''
            #${entry} {
              background-image: url("${png}");
            }
          ''
        );
        palette =
          (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
          .${config.catppuccin.flavor}.colors;
        backgroundRgb = palette.base.rgb;
      in
      #css
      ''
        * {
        	background-image: none;
        }

        window {
        	background-color: rgba(${toString backgroundRgb.r}, ${toString backgroundRgb.g}, ${toString backgroundRgb.b}, 0.9);
        }

        button {
          color: ${palette.text.hex};
        	background-color: ${palette.surface0.hex};
          border-color: ${palette.lavender.hex};
        	border-style: solid;
        	border-width: 2px;
        	background-repeat: no-repeat;
        	background-position: center;
        	background-size: 25%;
        }

        button:focus, button:hover, button:active {
        	background-color: ${palette.surface2.hex};
        	outline-style: none;
        }

        ${convertIcon "lock" "system-lock-screen"}
        ${convertIcon "hibernate" "system-suspend-hibernate"}
        ${convertIcon "logout" "system-log-out"}
        ${convertIcon "shutdown" "system-shutdown"}
        ${convertIcon "suspend" "system-suspend"}
        ${convertIcon "reboot" "system-reboot"}
      '';
  };
}
