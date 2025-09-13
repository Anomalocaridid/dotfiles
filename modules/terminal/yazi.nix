{ inputs, ... }:
{
  # Yazi plugins options module
  flake-file.inputs.nix-yazi-plugins = {
    url = "github:lordkekz/nix-yazi-plugins";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  unify.modules.general = {
    nixos.nixpkgs.overlays = [ inputs.nix-yazi-plugins.overlays.default ];
    home =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        # For some reason, using ${pkgs.system} for the system causes infinite recursion
        imports = [ inputs.nix-yazi-plugins.legacyPackages.x86_64-linux.homeManagerModules.default ];

        xdg.mimeApps.defaultApplications."inode/directory" = "yazi.desktop";

        programs.yazi = {
          enable = true;
          enableFishIntegration = true;
          plugins = {
            # Snippets
            "smart-paste" = pkgs.writeTextFile rec {
              name = "main.lua";
              destination = "/${name}";
              text =
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
          };
          settings.opener.open = [
            {
              run = ''xdg-open "$1"'';
              desc = "Open";
              orphan = true; # Ensure it stays open after yazi is closed
            }
          ];
          keymap = {
            input.prepend_keymap = [
              {
                on = [ "<Esc>" ];
                run = "close";
                desc = "Cancel input";
              }
            ];
            mgr.prepend_keymap = [
              {
                on = [ "p" ];
                run = "plugin smart-paste";
                desc = "Paste into the hovered directory or CWD";
              }
              {
                on = [ "<C-n>" ];
                run = ''shell -- ${lib.getExe pkgs.xdragon} --and-exit --all --on-top "$@"'';
                desc = "Drag and drop selected files with dragon";
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
              ouch.enable = true;
              exifaudio = {
                enable = true;
                # Provides more accurate metadata
                mediainfo.enable = true;
              };
              yatline = {
                enable = true;
                theme = {
                  name = "catppuccin";
                  settings = config.catppuccin.flavor;
                };
                addons.githead = {
                  enable = true;
                  settings = {
                    branch_symbol = "";
                    commit_symbol = "󰜘";
                    behind_symbol = "";
                    ahead_symbol = "";
                    stashes_symbol = "󱉙";
                    state_symbol = "";
                    staged_symbol = "󰩍";
                    unstaged_symbol = "";
                    untracked_symbol = "";
                  };
                };
                settings = {
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
                };
              };
            };
          };
        };
      };
  };
}
