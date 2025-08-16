import ../_common/host.nix {
  hostname = "desktop";
  diskoConfig = import ../_common/disko.nix {
    disk = "/dev/disk/by-id/nvme-WDS100T3X0C-00SJG0_20477T805943";
    memory = "32G";
  };
  facterReportPath = ./facter.json;
}
