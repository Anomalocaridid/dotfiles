{ config, pkgs, ... }: {
  home.packages = with pkgs; [
    # Needed for QMK
    qmk
    # Needed for KMK
    tio # Helpful for debugging
    circup # Easily manage circuitpython libraries
  ];

  # TODO: Ensure master branch is cloned as well
  systemd.user.services.qmk-clone =
    let
      qmkRepoDir = "${config.home.homeDirectory}/qmk_firmware";
    in
    {
      Unit = {
        Description = "Ensure QMK repo is cloned";
        ConditionPathExists = "!${qmkRepoDir}/readme.md"; # Note: repo dir should already exist because it is persisted
      };
      Service =
        let
          gitBin = "${pkgs.git}/bin/git";
          qmkRepoURL = "Anomalocaridid/qmk_firmware.git";
        in
        {
          Type = "oneshot";
          ExecStart = "${gitBin} clone --recurse-submodules --branch personal-keymap https://github.com/${qmkRepoURL} ${qmkRepoDir}"; # Clone with https
          ExecStartPost = [
            "${gitBin} -C ${qmkRepoDir} remote set-url origin git@github.com:${qmkRepoURL}" # But switch to ssh after
            "${gitBin} -C ${qmkRepoDir} remote add upstream https://github.com/qmk/qmk_firmware" # And also set upstream
          ];
        };
      Install.WantedBy = [ "default.target" ];
    };
}
