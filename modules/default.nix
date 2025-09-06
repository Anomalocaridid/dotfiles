{
  unify.modules.general = {
    nixos =
      { pkgs, ... }:
      {
        boot.kernelPackages = pkgs.linuxKernel.packages.linux_xanmod_latest;

        time.timeZone = "America/New_York";

        # Select internationalisation properties.
        i18n.defaultLocale = "en_US.UTF-8";

        # use xkbOptions in tty.
        console.useXkbConfig = true;
      };

    home =
      { config, pkgs, ... }:
      {
        home = {
          homeDirectory = "/home/${config.home.username}";
          packages = with pkgs; [
            itd
            killall
            pavucontrol # Graphical audio controller
            qalculate-gtk
            tree
          ];
        };
      };
  };
}
