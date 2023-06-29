{ pkgs, ... }: {
  # Custom GRUB theme
  boot.loader.grub.theme = pkgs.custom.cyberre-grub-theme;

  # TTY theming
  console = {
    font = "FiraCode Nerd Font";
    colors = [
      # Normal
      "000b1e"
      "ff0000"
      "d300c4"
      "f57800"
      "123e7c"
      "711c91"
      "0abdc6"
      "0abdc6"

      # Bright
      "1c61c2"
      "ff0000"
      "d300c4"
      "f57800"
      "00ff00"
      "711c91"
      "0abdc6"
      "d7d7d5"
    ];
  };

  # Configure Qt theme
  qt = rec {
    enable = true;
    platformTheme = "gtk2";
    style = platformTheme;
  };
}
