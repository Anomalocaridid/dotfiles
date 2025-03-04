{
  desktop = import ./common.nix {
    disk = "/dev/disk/by-id/nvme-WDS100T3X0C-00SJG0_20477T805943";
    memory = "32G";
  };
  laptop = import ./common.nix {
    disk = "/dev/disk/by-id/nvme-WD_BLACK_SN770_500GB_23313J808877";
    memory = "16G";
  };
}
