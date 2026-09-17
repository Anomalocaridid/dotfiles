{ config, ... }:
{
  flake.modules = {
    nixos.general = { pkgs, ... }: {
      nixpkgs.config.allowUnfreePackages = [ "hplip" ];

      services = {
        # Enable CUPS to print documents.
        printing = {
          enable = true;
          drivers = with pkgs; [ hplipWithPlugin ];
        };

        # Enable autodiscovery of network printers
        avahi = {
          enable = true;
          nssmdns4 = true;
          openFirewall = true;
        };
      };

      # CUPS config
      environment.persistence.${config.flake.meta.persistDir}.directories = [ "/var/lib/cups" ];
    };

    homeManager.general = { pkgs, ... }: {
      home.packages = with pkgs; [ hplipWithPlugin ];
    };
  };
}
