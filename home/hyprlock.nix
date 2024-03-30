{ config, inputs, ... }: {
  programs.hyprlock =
    let
      transparent = "0x00000000";
      opacity = "0x85";
    in
    {
      enable = true;

      sources = [
        (inputs.catppuccin-hyprland + "/themes/${config.catppuccin.flavour}.conf")
      ];
      general = {
        grace = 2;
        # Fade out takes a bit too long
        no_fade_out = true;
      };
      backgrounds = [
        {
          color = transparent;
        }
      ];

      input-fields = [
        # Center circle
        {
          size = { width = 300; height = 250; };
          outline_thickness = 15;
          outer_color = "${opacity}$lavenderAlpha";
          inner_color = "$base";
          font_color = "$text";
          fade_on_empty = false;
          placeholder_text = "";
          hide_input = true;
          check_color = "${opacity}$blueAlpha";
          fail_color = "${opacity}$maroonAlpha";
          fail_text = "";
          capslock_color = "${opacity}$peachAlpha";
          numlock_color = "${opacity}$yellowAlpha";
          bothlock_color = "${opacity}$pinkAlpha";

          position = { x = 0; y = 0; };
        }
        # Password dots
        {
          outline_thickness = 1;
          outer_color = transparent;
          inner_color = transparent;
          font_color = "$text";
          placeholder_text = "";
          check_color = "${opacity}$blueAlpha";
          fail_color = "${opacity}$maroonAlpha";
          capslock_color = transparent;
          numlock_color = transparent;
          bothlock_color = transparent;
          position = { x = 0; y = -85; };
        }
      ];

      labels = [
        # Failure attempts
        {
          text = "$ATTEMPTS[]";
          color = "$maroon";
          font_size = 20;

          position = { x = 0; y = 85; };
        }
        # Time
        {
          text = "cmd[update:200] date +'%r'";
          color = "$text";
          font_size = 30;

          position = { x = 0; y = 0; };
        }
        # Date
        {
          text = "cmd[update:1000] date +'%a, %x'";
          color = "$text";
          font_size = 20;

          position = { x = 0; y = -50; };
        }
      ];
    };
}
