{
  "/persist" = {
    hideMounts = true;
    directories = [
      "/etc/nixos" # Nix config
      "/etc/NetworkManager/system-connections" # Network connections
      "/var/lib/clamav" # ClamAV signature database
      "/var/lib/libvirt" # Virtual machine data
    ];
  };
}
