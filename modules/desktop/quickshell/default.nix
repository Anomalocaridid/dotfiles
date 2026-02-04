{ inputs, ... }:
{
  flake-file.inputs.qml-niri = {
    url = "github:imiric/qml-niri";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-parts.follows = "flake-parts";
    };
  };

  unify.modules.general.home =
    { pkgs, ... }:
    {
      programs.quickshell = {
        enable = true;
        # systemd.enable = true;
        configs.default = ./config;
        package = inputs.qml-niri.packages.${pkgs.stdenv.hostPlatform.system}.quickshell;
      };
    };
}
