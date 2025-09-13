inputs@{ flake-parts, ... }:
flake-parts.lib.mkFlake { inherit inputs; } {
  imports = [
    (inputs.import-tree [
      ./hosts
      ./modules
      ./scripts
    ])
    ./pkgs
  ];

  systems = [ "x86_64-linux" ];
}
