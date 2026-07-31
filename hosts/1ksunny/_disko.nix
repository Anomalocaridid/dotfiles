{ disk, memory }:
{ persistDir }:
{
  disko.devices = {
    disk.main = {
      type = "disk";
      device = disk;
      content = {
        type = "gpt";
        partitions =
          let
            bootMountOptions = [
              "noatime"
              "nodiratime"
            ];
          in
          {
            # Needed for Raspberry Pi 5
            FIRMWARE = {
              label = "FIRMWARE";
              priority = 1;
              type = "0700"; # Microsoft basic data
              attributes = [ 0 ]; # Required Partition
              size = "1024M";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot/firmware";
                mountOptions = bootMountOptions;
              };
            };

            ESP = {
              label = "ESP";
              type = "EF00"; # EFI System Partition (ESP)
              attributes = [ 2 ]; # Legacy BIOS Bootable, for U-Boot to find extlinux config
              size = "1024M";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = bootMountOptions ++ [ "umask=0077" ];
              };
            };

            root = {
              size = "100%";
              content = {
                type = "lvm_pv";
                vg = "pool";
              };
            };
          };
      };
    };

    lvm_vg.pool = {
      type = "lvm_vg";
      lvs = {
        swap = {
          size = memory;
          content.type = "swap";
        };
        persist = {
          # Uses different format for specifying size
          # Based on `lvcreate` arguments
          size = "100%FREE";
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

    nodev."/" = {
      fsType = "tmpfs";
      mountOptions = [
        "defaults"
        "mode=755"
      ];
    };
  };
}
