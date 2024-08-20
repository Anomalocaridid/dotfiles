{
  desktop = import ./common.nix {
    disk = "/dev/nvme0n1";
    memory = "32G";
  };
}
