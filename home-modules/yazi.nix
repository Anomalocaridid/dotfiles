{
  lib,
  pkgs,
  inputs,
  ...
}:
{
  imports = [ inputs.nix-yazi-plugins.legacyPackages.x86_64-linux.homeManagerModules.default ];

  programs.yazi = {
    enable = true;
    enableFishIntegration = true;
    plugins =
      let
        writePlugin =
          text:
          pkgs.writeTextFile rec {
            inherit text;
            name = "main.lua";
            destination = "/${name}";
          };
      in
      {
        # Previewers
        "glow" = pkgs.yaziPlugins.glow;
        "exifaudio" = pkgs.yaziPlugins.exifaudio;
        "ouch" = pkgs.yaziPlugins.ouch;
        # UI enhancements
        "smart-enter" = pkgs.yaziPlugins.smart-filter.src + /smart-enter.yazi;
        # Snippets
        "arrow" =
          writePlugin
            # lua
            ''
              --- @sync entry
              return {
                entry = function(_, job)
                  local current = cx.active.current
                  local new = (current.cursor + job.args[1]) % #current.files
                  ya.manager_emit("arrow", { new - current.cursor })
                end,
              }
            '';
        "smart-paste" =
          writePlugin
            # lua
            ''
              --- @sync entry
              return {
                entry = function()
                  local h = cx.active.current.hovered
                  if h and h.cha.is_dir then
                    ya.manager_emit("enter", {})
                    ya.manager_emit("paste", {})
                    ya.manager_emit("leave", {})
                  else
                    ya.manager_emit("paste", {})
                  end
                end,
              }
            '';
      };
    settings = {
      plugin = {
        prepend_previewers =
          [
            {
              name = "*.md";
              run = "glow";
            }
            {
              mime = "audio/*";
              run = "exifaudio";
            }
          ]
          ++ (builtins.map
            (type: {
              mime = "application/${type}";
              run = "ouch";
            })
            [
              "*zip"
              "x-tar"
              "x-bzip2"
              "x-7z-compressed"
              "x-rar"
              "x-xz"
            ]
          );
      };
      opener.open = [
        {
          run = ''xdg-open "$1"'';
          desc = "Open";
          orphan = true; # Ensure it stays open after yazi is closed
        }
      ];
    };
    keymap = {
      input.prepend_keymap = [
        {
          on = [ "<Esc>" ];
          run = "close";
          desc = "Cancel input";
        }
      ];
      manager.prepend_keymap =
        [
          {
            on = [ "l" ];
            run = "plugin smart-enter";
            desc = "Enter the child directory, or open the file";
          }
          {
            on = [ "p" ];
            run = "plugin smart-paste";
            desc = "Paste into the hovered directory or CWD";
          }
          {
            on = [ "<C-n>" ];
            run = ''shell '${lib.getExe pkgs.xdragon} --and-exit --all --on-top "$@"' --confirm'';
            desc = "Drag and drop selected files with dragon";
          }
          {
            on = [ "k" ];
            run = "plugin arrow -1";
            desc = "Move cursor up (wrapping)";
          }
          {
            on = [ "j" ];
            run = "plugin arrow 1";
            desc = "Move cursor down (wrapping)";
          }
        ]
        ++ (builtins.genList (
          x:
          let
            i = toString (x + 1);
          in
          {
            on = [ "${i}" ];
            run = "plugin relative-motions --args=${i}";
            desc = "Move in relative steps";
          }
        ) 9);
    };

    yaziPlugins = {
      enable = true;
      plugins = {
        starship.enable = true;
        full-border.enable = true;
        relative-motions = {
          enable = true;
          show_numbers = "relative_absolute";
          show_motion = true;
        };
        smart-filter.enable = true;
      };
      extraConfig =
        #lua
        ''
          function Status:name()
            local h = cx.active.current.hovered
            if not h then
              return ui.Span("")
            end

            -- Modified to show symlink in status bar
            local linked = ""
            if h.link_to ~= nil then
              linked = " -> " .. tostring(h.link_to)
            end
            return ui.Span(" " .. h.name .. linked)
          end

          -- Add user/group of files in status bar
          Status:children_add(function()
            local h = cx.active.current.hovered
            if h == nil or ya.target_family() ~= "unix" then
              return ui.Line {}
            end

            return ui.Line {
              ui.Span(ya.user_name(h.cha.uid) or tostring(h.cha.uid)):fg("magenta"),
              ui.Span(":"),
              ui.Span(ya.group_name(h.cha.gid) or tostring(h.cha.gid)):fg("magenta"),
              ui.Span(" "),
            }
          end, 500, Status.RIGHT)
        '';
    };
  };

  # Dependencies for plugins
  home.packages = with pkgs; [
    # Previewers
    glow
    exiftool
    ouch
  ];
}
