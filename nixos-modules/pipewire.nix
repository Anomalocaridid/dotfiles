{ ... }:
{
  # Disable pulseaudio in order to use pipewire
  hardware.pulseaudio.enable = false;
  # Needed for pipewire to work in real time
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
    lowLatency.enable = true;
  };
}
