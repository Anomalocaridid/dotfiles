final: prev: {
  # Custom-written packages
  custom = {
    # custom locking script
    lockman = final.callPackage ./lockman { };
    # custom screenshot script
    screenshot = final.callPackage ./screenshot { };
  };

  # Generated sources
  sources = builtins.mapAttrs (_: p: p.src) (
    (import ../_sources/generated.nix) {
      inherit (final)
        fetchurl
        fetchgit
        fetchFromGitHub
        dockerTools
        ;
    }
  );
}
