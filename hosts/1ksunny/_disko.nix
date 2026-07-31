{ disk }:
{ persistDir }:
{
  disko.devices = {
    disk.main = {
      type = "disk";
      device = disk;
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            priority = 1;
            name = "ESP";
            start = "1M";
            end = "128M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "umask=0077" ];
            };
          };
          root = {
            size = "100%";
            content = {
              type = "btrfs";
              extraArgs = [ "-f" ]; # Override existing partition
              # Subvolumes must set a mountpoint in order to be mounted
              # unless its parent is mounted
              subvolumes =
                let
                  mountOptions = [
                    "compress=zstd"
                    "noatime"
                    "nodiratime"
                    "discard"
                  ];
                in
                {
                  "/nix" = {
                    inherit mountOptions;
                    mountpoint = "/nix";
                  };
                  "/persist" = {
                    inherit mountOptions;
                    mountpoint = "/persist";
                  };
                  "/log" = {
                    inherit mountOptions;
                    mountpoint = "/var/log";
                  };
                };
            };
          };

        };
      };
    };
    nodev."/" = {
      fsType = "tmpfs";
      mountOptions = [
        "defaults"
        "mode=755"
      ];
    };
  };
}
