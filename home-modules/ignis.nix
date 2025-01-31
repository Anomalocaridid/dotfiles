{ lib, pkgs, ... }:
let
  ignisPackage = pkgs.ignis.overrideAttrs (oldAttrs: {
    propagatedBuildInputs =
      (oldAttrs.propagatedBuildInputs or [ ])
      ++ (with pkgs; [
        (python312.withPackages (
          ps: with ps; [
            psutil
          ]
        ))
      ]);
  });
in
{
  home.packages = [
    ignisPackage
  ];

  xdg.configFile.ignis.source = ./.config/ignis;

  # This lets the Ignis bar count as a tray for programs that rely on tray.target
  systemd.user.services.ignis = {
    Unit = {
      Description = "Ignis bar";
      PartOf = [
        "tray.target"
      ];
      # NOTE: will error if wayland compositor is not started first
      # NOTE: will also cause dependency issues if after graphical-session.target
      # TODO: make window-manager-agnostic
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
}
