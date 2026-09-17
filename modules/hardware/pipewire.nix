{ inputs, ... }:
{
  flake.modules = {
    nixos = {
      desktop = {
        imports = [ inputs.nix-gaming.nixosModules.pipewireLowLatency ];
        services.pipewire.lowLatency.enable = true;
      };

      default = {
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
        };
      };
    };

    homeManager.general = { pkgs, ... }: {
      # Graphical audio controller
      home.packages = with pkgs; [ pavucontrol ];
    };
  };
}
