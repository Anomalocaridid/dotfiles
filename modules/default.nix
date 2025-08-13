{ inputs, ... }:
{
  unify.modules.default = {
    nixos =
      { lib, pkgs, ... }:
      {
        imports = [ inputs.disko.nixosModules.disko ];

        boot = {
          kernelPackages = pkgs.linuxKernel.packages.linux_xanmod_latest;
          # Enable KVM nested virtualization
          extraModprobeConfig = "options kvm_amd nested=1";
        };

        networking.networkmanager.enable = true;

        time.timeZone = "America/New_York";

        # Select internationalisation properties.
        i18n.defaultLocale = "en_US.UTF-8";

        # use xkbOptions in tty.
        console.useXkbConfig = true;

        # Enable AMD microcode updates
        hardware.enableRedistributableFirmware = true;

        # Enable CUPS to print documents.
        services.printing.enable = true;

        # Tell electron apps to use Wayland
        environment.sessionVariables.NIXOS_OZONE_WL = "1";
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
            strawberry
            tree
            tuxpaint
            wl-clipboard
          ];

          sessionVariables = {
            # Some things require $EDITOR to be a single command with no args
            EDITOR = "xdg-open";
            VISUAL = "$EDITOR";
            PAGER = "bat";
            MANPAGER = "sh -c 'col --no-backspaces --spaces | bat --plain --language=man'";
          };
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
