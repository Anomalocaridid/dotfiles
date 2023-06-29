{ pkgs, ... }: {
  # Enable KVM nested virtualization
  boot.extraModprobeConfig = "options kvm_amd nested=1";
  # Enable libvirtd for virt-manager
  virtualisation.libvirtd.enable = true;
  # Needed to configure virt-manager
  programs.dconf.enable = true;

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    virt-manager
  ];
}
