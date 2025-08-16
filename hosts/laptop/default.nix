import ../_common/host.nix {
  hostname = "laptop";
  disk = "/dev/disk/by-id/nvme-WD_BLACK_SN770_500GB_23313J808877";
  memory = "16G";
  facterReportPath = ./facter.json;
}
