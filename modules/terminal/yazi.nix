{
  unify.modules.general.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      xdg.mimeApps.defaultApplications."inode/directory" = "yazi.desktop";

      # Allow for overriding theme elements
      # TODO: remove when catppuccin/nix updates yazi module to put files in flavor directory
      catppuccin.yazi.enable = false;

      xdg.configFile = with config.catppuccin; {
        "yazi/flavors/catppuccin.yazi/flavor.toml".source =
          "${sources.yazi}/${flavor}/catppuccin-${flavor}-${accent}.toml";

        "yazi/flavors/catppuccin.yazi/tmtheme.xml".source =
          "${sources.bat}/Catppuccin ${lib.toSentenceCase flavor}.tmTheme";
      };

      programs.yazi = {
        enable = true;
        enableFishIntegration = true;
        # Needed because `home.stateVersion` < 26.05
        shellWrapperName = "y";

        extraPackages = with pkgs; [
          # exifaudio.yazi
          exiftool
          mediainfo
          # ouch.yazi
          ouch
        ];

        plugins = {
          full-border = {
            package = pkgs.yaziPlugins.full-border;
            setup = true;
          };
          mediainfo = pkgs.yaziPlugins.mediainfo;
          ouch = pkgs.yaziPlugins.ouch;
          piper = pkgs.yaziPlugins.piper;
          relative-motions = {
            package = pkgs.yaziPlugins.relative-motions;
            setup = true;
            settings = {
              show_numbers = "relative_absolute";
              show_motion = true;
            };
          };
          smart-enter = pkgs.yaziPlugins.smart-enter;
          smart-filter = pkgs.yaziPlugins.smart-filter;
          smart-paste = pkgs.yaziPlugins.smart-paste;
          # Configured directly in init.lua
          yatline = pkgs.yaziPlugins.yatline;
          yatline-catppuccin = pkgs.yaziPlugins.yatline-catppuccin;
          yatline-githead = pkgs.yaziPlugins.yatline-githead;
        };

        settings = {
          opener.open = [
            {
              run = ''xdg-open "$1"'';
              desc = "Open";
              orphan = true; # Ensure it stays open after yazi is closed
            }
          ];

          plugin =
            let
              shared_settings = [
                # Replace magick, image, video with mediainfo
                {
                  mime = "{audio,video,image}/*";
                  run = "mediainfo";
                }
              ];
            in
            {
              prepend_preloaders = shared_settings;
              prepend_previewers = shared_settings ++ [
                {
                  mime = "application/{*zip,tar,bzip2,7z*,rar,xz,zstd,java-archive}";
                  run = "ouch --archive-icon=' ' --show-file-icons";
                }
                {
                  url = "*.md";
                  run = ''piper -- CLICOLOR_FORCE=1 ${lib.getExe pkgs.glow} --width $w --style ${config.home.sessionVariables.GLAMOUR_STYLE} "$1"'';
                }
              ];
            };
        };

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
              on = [ "<C-n>" ];
              run = ''shell -- ${lib.getExe pkgs.dragon-drop} --and-exit --all --on-top "$@"'';
              desc = "Drag and drop selected files with dragon";
            }
            {
              on = [ "C" ];
              run = "plugin ouch";
              desc = "Compress with ouch";
            }
            {
              on = [ "F" ];
              run = "plugin smart-filter";
              desc = "Smart filter";
            }
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
          ]
          # Needed for relative-motions.yazi
          ++ (builtins.genList (
            x:
            let
              distance = builtins.toString (x + 1);
            in
            {
              on = [ distance ];
              run = "plugin relative-motions ${distance}";
              desc = "Move in relative steps";
            }
          ) 10);
        };

        theme = {
          # TODO: remove when catppuccin/nix updates yazi module to put files in flavor directory and sets this
          flavor.dark = "catppuccin";
          indicator.padding = {
            # Compensate for relative-motions's indicator clashing visually with indicator
            open = "";
            close = "";
          };
        };

        initLua =
          let
            yatlineSettings = lib.generators.toLua { } {
              # Note that this relies on variable name in init.lua
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
            };
            yatlineGitheadSettings = lib.generators.toLua { } {
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
            };
          in
          # lua
          ''
            -- Update zoxide database when changing directories
            -- It's a built-in plugin, so it has to be set directly
            require("zoxide"):setup{ update_db = true; }

            local yatline_theme = require("yatline-catppuccin"):setup(${config.catppuccin.flavor})

            require("yatline"):setup(${yatlineSettings})

            require("yatline-githead"):setup(${yatlineGitheadSettings})
          '';
      };
    };
}
