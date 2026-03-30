{ inputs, ... }:
{
  unify.modules.general.home =
    { config, pkgs, ... }:
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

      programs.librewolf = {
        nativeMessagingHosts = with pkgs; [ keepassxc ];
        policies.ExtensionSettings =
          let
            id = "keepassxc-browser@keepassxc.org";
          in
          {
            ${id} = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";
              installation_mode = "force_installed";
              private_browsing = true;
            };
          };
      };

      # Ensure password database is always selected
      home.file =
        let
          filename = "keepassxc.ini";
        in
        {
          "${config.xdg.cacheHome}/keepassxc/${filename}".source =
            (inputs.nixago.lib.${pkgs.stdenv.hostPlatform.system}.make {
              output = filename;
              data = {
                General.LastActiveDatabase = "${config.home.homeDirectory}/Sync/Keepass Databases/Personal.kdbx";
              };
            }).configFile;
        };
    };
}
