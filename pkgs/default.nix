final: prev: {
  # Custom-written packages
  custom = {
    # custom locking script
    lockman = final.callPackage ./lockman { };
    # custom screenshot script
    screenshot = final.callPackage ./screenshot { };
    # Icon recoloring library
    color-manager = final.callPackage ./color-manager { };
    basic-colormath = final.callPackage ./basic-colormath { };
    # Turns Wayland clients into wallpapers
    windowtolayer = final.callPackage ./windowtolayer { };
  };
}
