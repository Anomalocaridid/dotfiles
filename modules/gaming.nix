{ ... }: {
  programs = {
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };
    # On-demand system optimization for gaming
    gamemode.enable = true;
  };

  # Nintendo Pro Controller / Joycon support
  services.joycond.enable = true;
  # Support Direct Rendering for 32-bit applications, like Wine
  hardware.opengl.driSupport32Bit = true;
}
