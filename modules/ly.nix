{ ... }: {
  services = {
    # Needed for ly to work
    xserver.enable = true;

    displayManager.ly = {
      enable = true;
      settings = {
        animate = true;
        # animation = 0;
        blank_password = true;
        hide_borders = true;
      };
    };
  };
}
