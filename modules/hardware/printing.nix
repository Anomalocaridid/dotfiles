{ config, ... }:
{
  unify.modules.general = {
    nixos =
      { pkgs, ... }:
      {
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
        environment.persistence.${config.flake.meta.persistDir}.directories = [ "/etc/cups" ];
      };

    home =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ hplipWithPlugin ];
      };
  };
}
