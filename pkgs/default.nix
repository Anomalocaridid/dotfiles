final: prev: {
  # Custom-written packages
  custom = {
    # custom locking script
    lockman = final.callPackage ./lockman { };
    # custom screenshot script
    screenshot = final.callPackage ./screenshot { };
  };
}
