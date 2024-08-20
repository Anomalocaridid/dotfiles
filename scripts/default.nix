{ pkgs, inputs', ... }:
rec {
  install = pkgs.callPackage ./install.nix {
    inherit generate-hardware-config chpasswd;
    disko = inputs'.disko.packages.disko;
  };
  generate-hardware-config = pkgs.callPackage ./generate-hardware-config.nix { };
  chpasswd = pkgs.callPackage ./chpasswd.nix { };
}
