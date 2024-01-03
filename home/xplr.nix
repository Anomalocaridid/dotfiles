{ lib, pkgs, inputs, ... }: {
  home.packages = with pkgs; [
    # Plugin dependencies
    xdragon
    dua
    ouch
    # Hack dependencies
    pipe-rename
  ];

  programs.xplr =
    let
      pluginArgs = [
        # Extension
        {
          name = "context-switch";
          bind = "csw";
        }
        { name = "offline-docs"; }
        {
          name = "tri-pane";
          args = {
            left_pane_width.Percentage = 16;
            middle_pane_width.Percentage = 45;
            right_pane_width.Percentage = 19;
          };
        }
        { name = "type-to-nav"; }
        { name = "wl-clipboard"; }
        {
          name = "style";
          bind = "style";
          setup = false;
        }
        # Integration
        { name = "dragon"; }
        { name = "dua-cli"; }
        {
          name = "fzf";
          args = {
            enter_dir = true;
          };
        }
        { name = "scp"; }
        { name = "ouch"; }
        { name = "zoxide"; }
        # Theme
        { name = "icons"; }
        { name = "extra-icons"; }
      ];
      luaFormat = lib.generators.toLua { };
      renderPlugin = ({ name
                      , args ? null
                      , bind ? null
                      , setup ? true
                      }:
        let
          inherit (lib.strings) optionalString;
          bindName = optionalString (bind != null) "\n${bind}";
          setupCall = optionalString setup "${bindName}.setup(${luaFormat args})";
          binding = optionalString (bind != null) "local ${bind} = ";
        in
        #lua
        ''
          ${binding}require("${name}")${setupCall}
        ''
      );
    in
    {
      enable = true;
      plugins = lib.trivial.pipe pluginArgs
        [
          (map (plugin: {
            "${plugin.name}" = inputs."${plugin.name}.xplr";
          }))
          lib.attrsets.mergeAttrsList
        ];
      extraConfig = #lua
        # Generate code to load plugins based on attrs
        (lib.concatMapStrings renderPlugin pluginArgs) + #lua
        ''
          local old_render_right = xplr.fn.custom.tri_pane.render_right_pane
          -- Set right pane to preview files with bat
          local function render_right_pane(ctx)
            local n = ctx.app.focused_node
            -- Ensure that only directories and symlinks
            -- use default renderer
            if n.is_dir or n.is_symlink then
              return old_render_right(ctx)
            else
              local file = io.open("/tmp/xplr-preview.tmp")

              if file == nil then
                return old_render_right(ctx)
              end

              local i = 0
              local res = ""

              for line in file:lines() do
                res = res .. line .. "\n"
                -- Do not read lines that will not be seen
                -- Otherwise, significant slowdown for large files
                if i == ctx.layout_size.height then
                  break
                end

                i = i + 1
              end

              file:close()
              return res
              end
            end

          xplr.fn.custom.tri_pane.render_right_pane = render_right_pane

          -- Add context switch pane to tri-pane layout
          xplr.fn.custom.render_active_contexts = function(ctx)
            local res = ""
            local contexts = csw.get_contexts()

            for i = 1, #contexts do 
              local index = i + 1

              if i == #contexts then
                index = 1
              end

              if next(contexts[index]) == nil then
                res = res .. style.separator .. tostring(i)
              else
                res = res.. style.style.add_modifiers.Bold(style.style.fg.Indexed (i) (i))
              end

              res = res.. " "
            end

            return {
              CustomParagraph = {
                ui = {
                  title = { format = " Contexts " }
                },
                  body = res
              }
            }
          end

          local function merge(t1, t2)
            for k, v in pairs(t2) do
              if (type(v) == "table") and (type(t1[k] or false) == "table") then
                merge(t1[k], t2[k])
              else
                t1[k] = v
              end
            end
          end

          -- Customize layout
          -- Assumes tri-pane layout already set as default
          merge(xplr.config.layouts.builtin.default, {
            Vertical = {
              splits = {
                -- top row
                {
                  Horizontal = {
                    config = {
                      constraints = {
                        { Percentage = 16 },
                        { Percentage = 84 },
                      },
                    },
                    splits = {
                      { Dynamic = "custom.render_active_contexts" },
                      "SortAndFilter"
                    }
                  }
                },
                -- middle row
                {
                  Horizontal = {
                    config = {
                      constraints = {
                        [4] = { Percentage = 20 },
                      },
                    },
                    splits = {
                      [4] = {
                        Vertical = {
                          config = {
                            constraints = {
                              { Percentage = 75 },
                              { Percentage = 25 },
                            },
                          },
                          splits = {
                            "HelpMenu",
                            "Selection",
                          },
                        },
                      },
                    }
                  }
                }
              }
            }
          })

          -- Set extra-icons as column renderer
          xplr.config.general.table.row.cols[2] = {
            format = "custom.icons_dtomvan_col_1"
          }

          xplr.config.modes.builtin.default.key_bindings.on_key.R = {
            help = "batch rename",
            messages = {
              {
                BashExec = [===[
                  SELECTION=$(cat "''${XPLR_PIPE_SELECTION_OUT:?}")
                  NODES=''${SELECTION:-$(cat "''${XPLR_PIPE_DIRECTORY_NODES_OUT:?}")}
                  if [ "$NODES" ]; then
                    echo -e "$NODES" | renamer
                    "$XPLR" -m ExplorePwdAsync
                  fi
                ]===]
              }
            }
          }

          return {
            -- Needed for bat previews
            -- TODO: Figure out automatic wrapping
            -- TODO: add line range to bat
            -- FIXME: Still a bit slow if rapidly scrolling through files
            -- TODO: Figure out how to run as "async" background process
            on_focus_change = {
              {
                BashExecSilently = [===[
                  # Only generate previews for files
                  if [[ -f "$XPLR_FOCUS_PATH" ]]; then
                    # Workaround to keep bat from freezing on large binary files
                    # Exits with error code 124 when timeout
                    timeout 0.5 bat --plain --color=always "$XPLR_FOCUS_PATH" > /tmp/xplr-preview.tmp
                  fi
                ]===]
              }
            }
          }
        '';
    };

  # Fix xplr plugin sourcing
  # Remove when home-manager#4521 is merged
  disabledModules = [ "programs/xplr.nix" ];
  imports = [ "${inputs.hm-xplr-fix}/modules/programs/xplr.nix" ];
}


