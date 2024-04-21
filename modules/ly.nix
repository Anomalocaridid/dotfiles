{ ... }:
{
  services = {
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
