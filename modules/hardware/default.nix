{ inputs, ... }:
{
  flake-file.inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";

  unify.modules.laptop.nixos.imports = [ inputs.nixos-hardware.nixosModules.framework-16-7040-amd ];
}
