{ config, ... }:
let
  inherit (config.flake.meta) persistDir;
in
{
  unify.modules.general.nixos = {
    # Daily btrfs snapshots
    services.btrbk.instances.daily.settings = {
      snapshot_preserve = "14d";
      snapshot_preserve_min = "2d";
      volume.${persistDir} = {
        subvolume = ".";
        snapshot_dir = "btrbk_snapshots";
      };
    };

    # Create btrbk snapshot directory if it doesn't exist yet.
    systemd.services = {
      btrbk-daily-init =
        let
          btrbkServices = [ "btrbk-daily.service" ];
          snapshotDir = "${persistDir}/btrbk_snapshots";
        in
        {
          description = "Ensure btrbk snapshot dir exists";
          before = btrbkServices;
          script = "mkdir ${snapshotDir}";
          unitConfig.ConditionPathExists = "!${snapshotDir}";
          serviceConfig.Type = "oneshot";
          requiredBy = btrbkServices;
        };
    };
  };
}
