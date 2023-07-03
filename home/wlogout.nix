{ config, pkgs, ... }: {
  programs.wlogout = {
    enable = true;
    layout = [
      {
        label = "lock";
        action = "${pkgs.custom.lockman}/bin/lockman.sh";
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
        useIcon = (icon: #css
          ''
            #${icon} {
              background-image: url("${config.programs.wlogout.package}/share/wlogout/icons/${icon}.png");
            }
          '');
      in
      #css
      ''
        * {
        	background-image: none;
        }

        window {
        	background-color: rgba(0, 11, 30, 0.9);
        }

        button {
          color: #0abdc6;
        	background-color: #133e7c;
          border-color: #ea00d9;
        	border-style: solid;
        	border-width: 2px;
        	background-repeat: no-repeat;
        	background-position: center;
        	background-size: 25%;
        }

        button:focus, button:hover {
        	background-color: #321959;
        	outline-style: none;
        }

        button:active {
        	background-color: #711c91;
        	outline-style: none;
        }
      '' + (builtins.concatStringsSep "" (map useIcon [
        "lock"
        "logout"
        "suspend"
        "hibernate"
        "shutdown"
        "reboot"
      ]));
  };
}
