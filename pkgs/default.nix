final: prev: {
  # Custom-written packages
  custom = {
    # Icon recoloring library
    color-manager = final.callPackage ./color-manager { };
    basic-colormath = final.callPackage ./basic-colormath { };
    # Turns Wayland clients into wallpapers
    windowtolayer = final.callPackage ./windowtolayer { };
    # Swaylock fork with background plugin support
    swaylock-plugin = final.callPackage ./swaylock-plugin { };
  };
}
