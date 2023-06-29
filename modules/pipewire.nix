{ ... }: {
  # Disable pulseaudio in order to use pipewire
  hardware.pulseaudio.enable = false;
  # Needed for pipewire
  security.rtkit.enable = true;
  # Enable sound.
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
    jack.enable = true;
  };
}
