{ pkgs, inputs, ... }: {
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

  # Adds Proton GE to Steam
  # Does not work in main overlay
  # Results in infinite recursion due to referencing pkgs
  nixpkgs.overlays = [
    (_: prev: {
      steam = prev.steam.override {
        extraProfile = "export STEAM_EXTRA_COMPAT_TOOLS_PATHS='${inputs.nix-gaming.packages.${pkgs.system}.proton-ge}'";
      };
    })
  ];

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
