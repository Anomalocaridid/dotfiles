{
  config,
  lib,
  inputs,
  ...
}:
let
  inherit (config.flake.meta) persistDir wm;
in
{
  flake-file.inputs.ignis = {
    url = "github:linkfrg/ignis";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  unify.modules.general = {
    # Needed for mpris service to fetch art from url
    nixos.services.gvfs.enable = true;

    home =
      { config, pkgs, ... }:
      {
        imports = [ inputs.ignis.homeManagerModules.default ];
        programs.ignis = {
          enable = true;
          configDir = ./config;

          services = {
            audio.enable = true;
            network.enable = true;
          };

          sass = {
            enable = true;
            useDartSass = true;
          };

          extraPackages = with pkgs.python313Packages; [ psutil ];
        };

        xdg.configFile = {
          "ignis/wm.py".text =
            # python
            ''
              PERSIST_DIR = "${persistDir}"
              GAP_WIDTH = ${toString wm.gapMinusBorder}
              # Assume scrolling both up and down have the same cooldown
              SCROLL_COOLDOWN_MS = ${toString wm.mouseCooldownMs}
              WORKSPACES = ${toString wm.workspaces}
            '';

          "ignis/wm.scss".text =
            # scss
            ''
              @use "sass:string";

              @mixin border {
              	border: ${toString wm.borderWidth}px solid string.unquote("@accent_color");
              	border-radius: ${toString wm.windowCornerRadius}px;
              }
            '';
          # Needed to include above files
          ignis.recursive = true;
        };

        # This lets the Ignis bar count as a tray for programs that rely on tray.target
        # Loosely based off of waybar's service in home manager
        systemd.user.services.ignis = {
          Unit = {
            Description = "Ignis bar";
            PartOf = [
              config.wayland.systemd.target
              "tray.target"
            ];
            After = [ config.wayland.systemd.target ];
          };
          Service = {
            ExecStart = "${lib.getExe config.programs.ignis.finalPackage} init";
            Restart = "on-failure";
          };
          Install.WantedBy = [
            config.wayland.systemd.target
            "tray.target"
          ];
        };
      };
  };
}
