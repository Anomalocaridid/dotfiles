{ ... }: {
  environment.persistence = {
    "/persist" = {
      hideMounts = true;
      directories = [
        "/etc/NetworkManager/system-connections" # Network connections
        "/etc/nixos" # Nix config
        "/var/lib/clamav" # ClamAV signature database
        "/var/lib/libvirt" # Virtual machine data
      ];
      files = [
        "/etc/machine-id" # Unique system id for logging, etc.
      ];
    };
  };
  # Needed for root to access bind mounted dirs created by impermanence
  programs.fuse.userAllowOther = true;
}
