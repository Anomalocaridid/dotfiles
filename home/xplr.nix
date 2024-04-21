{ lib, pkgs, ... }:
{
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
      renderPlugin = (
        {
          name,
          args ? null,
          bind ? null,
          setup ? true,
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
      plugins = lib.trivial.pipe pluginArgs [
        (map (plugin: {
          "${plugin.name}" = pkgs.sources."${plugin.name}-xplr";
        }))
        lib.attrsets.mergeAttrsList
      ];
      extraConfig = # lua
        # Generate code to load plugins based on attrs
        (lib.concatMapStrings renderPlugin pluginArgs)
        # lua
        + ''
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
        '';
    };
}
