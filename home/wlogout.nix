{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.wlogout = {
    enable = true;
    layout = [
      {
        label = "lock";
        action = "loginctl lock-session";
        text = "Lock";
        keybind = "l";
      }
      {
        label = "hibernate";
        action = "systemctl hibernate";
        text = "Hibernate";
        keybind = "h";
      }
      {
        label = "logout";
        action = "loginctl terminate-user $USER";
        text = "lOgout";
        keybind = "o";
      }
      {
        label = "shutdown";
        action = "systemctl poweroff";
        text = "Shutdown";
        keybind = "s";
      }
      {
        label = "suspend";
        action = "systemctl suspend";
        text = "sUspend";
        keybind = "u";
      }
      {
        label = "reboot";
        action = "systemctl reboot";
        text = "Reboot";
        keybind = "r";
      }
    ];
    style =
      let
        palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
        recolorIcon = (
          color: icon:
          let
            iconFile = "${config.programs.wlogout.package}/share/wlogout/icons/${icon}.png";
            recolored = pkgs.runCommand "${icon}-recolored.png" { } ''
              ${lib.getExe' pkgs.imagemagick "convert"} ${iconFile} -alpha extract -background "${color}" -alpha shape $out
            '';
          in
          #css
          ''
            #${icon} {
              background-image: url("${recolored}");
            }
          ''
        );
      in
      #css
      ''
        * {
        	background-image: none;
        }

        window {
        	background-color: rgba(${
           lib.trivial.pipe palette.base.rgb [
             (builtins.map toString)
             (lib.strings.concatStringsSep ", ")
           ]
         }, 0.9);
        }

        button {
          color: #${palette.text.hex};
        	background-color: #${palette.surface0.hex};
          border-color: #${palette.lavender.hex};
        	border-style: solid;
        	border-width: 2px;
        	background-repeat: no-repeat;
        	background-position: center;
        	background-size: 25%;
        }

        button:focus, button:hover, button:active {
        	background-color: #${palette.surface2.hex};
        	outline-style: none;
        }

        ${lib.concatStrings (
          map (button: recolorIcon "#${palette.text.hex}" button.label) config.programs.wlogout.layout
        )}
      '';
  };
}
