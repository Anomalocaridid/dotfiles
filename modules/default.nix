{
  unify.modules.general = {
    nixos =
      { lib, pkgs, ... }:
      {
        boot = {
          kernelPackages = pkgs.linuxKernel.packages.linux_xanmod_latest;
          # Enable KVM nested virtualization
          extraModprobeConfig = "options kvm_amd nested=1";
        };

        time.timeZone = "America/New_York";

        # Select internationalisation properties.
        i18n.defaultLocale = "en_US.UTF-8";

        # use xkbOptions in tty.
        console.useXkbConfig = true;
      };

    home =
      {
        config,
        pkgs,
        osConfig,
        ...
      }:
      {
        home = {
          homeDirectory = "/home/${config.home.username}";
          packages = with pkgs; [
            exercism
            itd
            killall
            orca-slicer # 3D Printer Slicer
            pavucontrol # Graphical audio controller
            qalculate-gtk
            quickemu # Make and run VMs
            tree
            tuxpaint
            wl-clipboard
          ];
        };

        services = {
          cliphist.enable = true;
          syncthing = {
            enable = true;
            # TODO: Enable and figure out how to configure declaratively
            # tray.enable = true;
          };
        };
      };
  };
}
