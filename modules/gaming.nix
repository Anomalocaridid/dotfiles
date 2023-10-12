{ pkgs, inputs, ... }: {
  programs = {
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      # Add extra compatibility tools to Steam
      extraCompatPackages = [
        # For some reason, nix-gaming overlay does not work
        inputs.nix-gaming.packages.${pkgs.system}.proton-ge
      ];
    };
    # On-demand system optimization for gaming
    gamemode.enable = true;
  };

  # Nintendo Pro Controller / Joycon support
  services.joycond.enable = true;
  # Support Direct Rendering for 32-bit applications, like Wine
  hardware.opengl.driSupport32Bit = true;

  ssbm = {
    overlay.enable = true;
    cache.enable = true;
  };

  # nix-gaming cache
  nix.settings = {
    substituters = [ "https://nix-gaming.cachix.org" ];
    trusted-public-keys = [ "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4=" ];
  };
}
