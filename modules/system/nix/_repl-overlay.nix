# Implement gytis-ivaskevicius/flake-utils-plus's fup-repl in nix repl
info: final: prev:
let
  flake = builtins.getFlake "self";
  hostname = (builtins.head (builtins.match "([a-zA-Z0-9\\-]+)\n" (builtins.readFile /etc/hostname)));
  nixpkgs = import flake.inputs.nixpkgs { };
  nixpkgsOutput = removeAttrs (nixpkgs // nixpkgs.lib) [
    "options"
    "config"
  ];
in
{
  inherit flake;
}
// flake
// builtins
// flake.nixosConfigurations
// flake.nixosConfigurations.${hostname}
// nixpkgsOutput
// {
  getFlake = path: builtins.getFlake (toString path);
}
