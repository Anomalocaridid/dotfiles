{
  unify.modules.general.nixos = {
    nixpkgs.overlays = [
      (final: prev: {
        # Custom-written packages
        custom = {
          # Icon recoloring library
          color-manager = final.callPackage ./color-manager { };
          basic-colormath = final.callPackage ./basic-colormath { };
        };
      })
    ];
  };
}
