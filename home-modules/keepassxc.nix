{
  pkgs,
  config,
  inputs,
  ...
}:
let
  filename = "keepassxc.ini";
  path = "keepassxc/${filename}";
  mkKeepassConf =
    data:
    (inputs.nixago.lib.${pkgs.system}.make {
      output = filename;
      inherit data;
    }).configFile;
in
{
  home.packages = with pkgs; [
    keepassxc
  ];

  home.file."${config.xdg.cacheHome}/${path}".source = mkKeepassConf {
    General.LastActiveDatabase = "${config.home.homeDirectory}/Sync/Keepass Databases/Personal.kdbx";
  };

  xdg.configFile.${path}.source = mkKeepassConf {
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
}
