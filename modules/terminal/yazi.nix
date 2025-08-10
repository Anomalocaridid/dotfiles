{ inputs, ... }:
{
  flake.modules = {
    nixos.yazi.nixpkgs.overlays = [ inputs.nix-yazi-plugins.overlays.default ];
    homeManager.yazi =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        # For some reason, using ${pkgs.system} for the system causes infinite recursion
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
              "exifaudio" = pkgs.yaziPlugins.exifaudio;
              "ouch" = pkgs.yaziPlugins.ouch;
              # UI enhancements
              "yatline" = inputs."yatline.yazi";
              "yatline-catppuccin" = inputs."yatline-catppuccin.yazi";
              "yatline-githead" = inputs."yatline-githead.yazi";
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
              full-border.enable = true;
              glow.enable = true;
              relative-motions = {
                enable = true;
                show_numbers = "relative_absolute";
                show_motion = true;
              };
              smart-enter.enable = true;
              smart-filter.enable = true;
            };
            runtimeDeps = with pkgs; [
              # Previewers
              exiftool
              ouch
            ];
            extraConfig =
              let
                luaFormat = lib.generators.toLua { };
              in
              #lua
              ''
                local yatline_theme = require("yatline-catppuccin"):setup("${config.catppuccin.flavor}")

                require("yatline"):setup(${
                  luaFormat {
                    theme = lib.generators.mkLuaInline "yatline_theme";
                    show_background = false;

                    header_line = {
                      left = {
                        section_a = [
                          {
                            type = "line";
                            custom = false;
                            name = "tabs";
                            params = [ "left" ];
                          }
                        ];
                        section_b = [ ];
                        section_c = [ ];
                      };
                      right = {
                        section_a = [
                          {
                            type = "string";
                            custom = false;
                            name = "date";
                            params = [ " %a, %b %d, %Y" ];
                          }
                        ];
                        section_b = [
                          {
                            type = "string";
                            custom = false;
                            name = "date";
                            params = [ " %X" ];
                          }
                        ];
                        section_c = [
                          {
                            type = "coloreds";
                            custom = false;
                            name = "task_states";
                          }
                          {
                            type = "coloreds";
                            custom = false;
                            name = "task_workload";
                          }
                        ];
                      };
                    };

                    status_line = {
                      left = {
                        section_a = [
                          {
                            type = "string";
                            custom = false;
                            name = "tab_mode";
                          }
                        ];
                        section_b = [
                          {
                            type = "string";
                            custom = false;
                            name = "hovered_size";
                          }
                        ];
                        section_c = [
                          {
                            type = "string";
                            custom = false;
                            name = "hovered_path";
                          }
                          {
                            type = "coloreds";
                            custom = false;
                            name = "githead";
                          }
                          {
                            type = "coloreds";
                            custom = false;
                            name = "count";
                            # Add filter count
                            params = [ true ];
                          }
                        ];
                      };
                      right = {
                        section_a = [
                          {
                            type = "string";
                            custom = false;
                            name = "cursor_position";
                          }
                        ];
                        section_b = [
                          {
                            type = "string";
                            custom = false;
                            name = "cursor_percentage";
                          }
                        ];
                        section_c = [
                          {
                            type = "string";
                            custom = false;
                            name = "hovered_file_extension";
                            params = [ true ];
                          }
                          {
                            type = "coloreds";
                            custom = false;
                            name = "permissions";
                          }
                          {
                            type = "string";
                            custom = false;
                            name = "hovered_ownership";
                          }
                        ];
                      };
                    };
                  }
                })

                require("yatline-githead"):setup(${
                  luaFormat {
                    theme = lib.generators.mkLuaInline "yatline_theme";

                    branch_symbol = "";
                    commit_symbol = "󰜘";
                    behind_symbol = "";
                    ahead_symbol = "";
                    stashes_symbol = "󱉙";
                    state_symbol = "";
                    staged_symbol = "󰩍";
                    unstaged_symbol = "";
                    untracked_symbol = "";
                  }
                })
              '';
          };
        };
      };
  };
}
