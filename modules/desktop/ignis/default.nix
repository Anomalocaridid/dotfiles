{ lib, inputs, ... }:
{
  flake.modules = {
    # Required for ignis
    nixos.ignis.services.gvfs.enable = true;
    homeManager.ignis =
      { config, pkgs, ... }:
      let
        ignisPackage = inputs.ignis.packages.${pkgs.system}.ignis.override {
          extraPackages = with pkgs.python313Packages; [
            psutil
            unicodeit
          ];
        };
      in
      {
        home.packages = [
          ignisPackage
        ];

        xdg.configFile =
          let
            niriSettings = config.programs.niri.settings;
            borderWidth = toString niriSettings.layout.border.width;
          in
          {
            "ignis/wm.py".text =
              # python
              ''
                BORDER_WIDTH = ${borderWidth}
                GAP_WIDTH = ${toString niriSettings.layout.gaps} - BORDER_WIDTH
                # Assume scrolling both up and down have the same cooldown
                SCROLL_COOLDOWN_MS = ${toString niriSettings.binds."Mod+WheelScrollUp".cooldown-ms}
              '';

            "ignis/wm.scss".text =
              # scss
              ''
                @use "sass:string";

                @mixin border {
                	border: ${borderWidth}px solid string.unquote("@accent_color");
                  // NOTE: Depends on window-rule order, chooses corner that it should match up with
                	border-radius: ${toString (builtins.ceil (builtins.elemAt niriSettings.window-rules 0).geometry-corner-radius.top-right)}px;
                }
              '';
            ignis = {
              source = ./config/ignis;
              recursive = true;
            };
          };

        # This lets the Ignis bar count as a tray for programs that rely on tray.target
        systemd.user.services.ignis = {
          Unit = {
            Description = "Ignis bar";
            PartOf = [
              "tray.target"
            ];
            # NOTE: will error if wayland compositor is not started first
            # NOTE: will also cause dependency issues if after graphical-session.target
            After = "niri.service";
            BindsTo = "graphical-session.target";
          };
          Service = {
            ExecStart = "${lib.getExe ignisPackage} init";
            Restart = "on-failure";
          };
          Install.RequiredBy = [
            "tray.target"
          ];
        };
      };
  };
}
