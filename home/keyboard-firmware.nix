{
  config,
  lib,
  pkgs,
  ...
}:
let
  qmkRepoDir = "${config.home.homeDirectory}/qmk_userspace";
in
{
  home.packages = with pkgs; [
    # Needed for QMK
    qmk
    # Needed for KMK
    tio # Helpful for debugging
    circup # Easily manage circuitpython libraries
  ];

  # TODO: figure out a way to make qmk_firmware cloning reproducible

  xdg.configFile."qmk/qmk.ini".source =
    let
      iniFormat = pkgs.formats.ini { };
    in
    iniFormat.generate "qmk-config" { user.overlay_dir = qmkRepoDir; };

  systemd.user.services.qmk-clone = {
    Unit = {
      Description = "Ensure QMK repo is cloned";
      ConditionPathExists = "!${qmkRepoDir}/readme.md"; # Note: repo dir should already exist because it is persisted
    };
    Service =
      let
        gitBin = lib.getExe pkgs.git;
        qmkRepoURL = "Anomalocaridid/qmk_userspace.git";
      in
      {
        Type = "oneshot";
        ExecStart = "${gitBin} clone https://github.com/${qmkRepoURL} ${qmkRepoDir}"; # Clone with https
        ExecStartPost = [
          "${gitBin} -C ${qmkRepoDir} remote set-url origin git@github.com:${qmkRepoURL}" # But switch to ssh after
        ];
      };
    Install.WantedBy = [ "default.target" ];
  };
}
