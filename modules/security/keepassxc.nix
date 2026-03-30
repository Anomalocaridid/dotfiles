{ inputs, ... }:
{
  unify.modules.general.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      programs.keepassxc = {
        enable = true;
        settings = {
          General = {
            ConfigVersion = 2;
            MinimizeAfterUnlock = true;
          };
          Browser = {
            Enabled = true;
            UpdateBinaryPath = false;
          };
          GUI = {
            # Use system theme
            ApplicationTheme = "classic";
            # Compact toolbar
            CompactMode = true;
            # Minimize to tray icon on close
            MinimizeOnClose = true;
            MinimizeToTray = true;
            ShowTrayIcon = true;
            TrayIconAppearance = "colorful";
          };
          PasswordGenerator = {
            AdvancedMode = true;
            Dashes = true;
            Length = 128;
            Logograms = true;
            Math = true;
          };
          Security = {
            # Use DuckDuckGo to download favicons for privacy
            IconDownloadFallback = true;
            LockDatabaseIdle = true;
          };
        };
      };

      # Ensure password database is always selected
      home.file = {
        "${config.xdg.cacheHome}/keepassxc/keepassxc.ini".source =
          (inputs.nixago.lib.${pkgs.stdenv.hostPlatform.system}.make {
            output = "keepassxc.ini";
            data = {
              General.LastActiveDatabase = "${config.home.homeDirectory}/Sync/Keepass Databases/Personal.kdbx";
            };
          }).configFile;
      };

      xdg.autostart.entries = lib.singleton (
        pkgs.makeDesktopItem {
          name = "keepassxc-autostart";
          desktopName = "KeepassXC (Autostart)";
          exec = "${lib.getExe config.programs.keepassxc.package} --minimized";
          # Make it more concise to get path to desktop file
          destination = "/";
        }
        + "/keepassxc-autostart.desktop"
      );
    };
}
