{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.librewolf =
    let
      inherit (pkgs.nur.repos.rycee) firefox-addons;
      catppuccin-firefox = firefox-addons.buildFirefoxXpiAddon {
        pname = "catppuccin-firefox";
        version = "old";
        url = "https://github.com/catppuccin/firefox/releases/download/old/catppuccin_${config.catppuccin.flavor}_${config.catppuccin.accent}.xpi";
        sha256 = "sha256-pSlzVe7XbTbrC76iAinYrr7qIl69OpH3Wk00MoAIe74=";
        addonId = "{76aabc99-c1a8-4c1e-832b-d4f2941d5a7a}";
        meta = { };
      };
    in
    {
      enable = true;
      nativeMessagingHosts = with pkgs; [
        keepassxc
      ];
      package = pkgs.librewolf.override {
        # Set preferences here to ensure they stay set
        extraPrefs = # javascript
          ''
            // Only sync bookmarks and tabs
            // Some of these are set by default, but they should be locked just in case
            lockPref("services.sync.engine.bookmarks", true)
            lockPref("services.sync.engine.tabs", true)
            lockPref("services.sync.engine.addons", false);
            lockPref("services.sync.engine.addresses", false);
            lockPref("services.sync.engine.addresses.available", false);
            lockPref("services.sync.engine.creditcards", false);
            lockPref("services.sync.engine.creditcards.available", false);
            lockPref("services.sync.engine.history", false);
            lockPref("services.sync.engine.passwords", false);
            lockPref("services.sync.engine.prefs", false);
            // Enable sync
            lockPref("identity.fxaccounts.enabled", true);
            // Automatically enable installed extensions
            lockPref("extensions.autoDisableScopes", 0);
            // Set theme
            lockPref("extensions.activeThemeID", "${catppuccin-firefox.addonId}");
            // Use same search engine for private browsing
            lockPref("browser.search.separatePrivateDefault", false);
            // Clear history on shutdown
            // This should be on by default but is not for some reason
            lockPref("privacy.clearOnShutdown.history", true);
            lockPref("privacy.clearOnShutdown_v2.historyFormDataAndDownloads", true);
          '';
      };
      profiles = {
        # Separate profile to enable WebGL because it allows more fingerprinting
        gaming = {
          id = 1;
          # Un-break certain websites
          settings."webgl.disabled" = false;
          extensions = with firefox-addons; [
            canvasblocker # Recommended for enabled WebGL
            keepassxc-browser
          ];
        };
        default = {
          extensions = with firefox-addons; [
            awesome-rss
            catppuccin-firefox
            catppuccin-gh-file-explorer
            darkreader
            indie-wiki-buddy
            keepassxc-browser
            redirector
            stylus
            ublacklist
          ];
          search =
            let
              mkParams = lib.mapAttrsToList lib.nameValuePair;
            in
            {
              engines = {
                "Nix Packages" = {
                  urls = [
                    {
                      template = "https://search.nixos.org/packages";
                      params = mkParams {
                        channel = "unstable";
                        type = "packages";
                        query = "{searchTerms}";
                      };
                    }
                  ];
                  iconUpdateURL = "https://search.nixos.org/favicon.png";
                  updateInterval = 24 * 60 * 60 * 1000; # every day
                  definedAliases = [ "@np" ];
                };

                "NixOS Wiki" = {
                  urls = [
                    {
                      template = "https://wiki.nixos.org/index.php";
                      params = mkParams {
                        search = "{searchTerms}";
                      };
                    }
                  ];
                  iconUpdateURL = "https://wiki.nixos.org/favicon.ico";
                  updateInterval = 24 * 60 * 60 * 1000; # every day
                  definedAliases = [ "@nw" ];
                };

                "Nix Home Manager" = {
                  urls = [
                    {
                      template = "https://home-manager-options.extranix.com/";
                      params = mkParams {
                        query = "{searchTerms}";
                        release = "master";
                      };
                    }
                  ];
                  iconUpdateURL = "https://home-manager-options.extranix.com/images/favicon.png";
                  updateInterval = 24 * 60 * 60 * 1000; # every day
                  definedAliases = [ "@nhm" ];
                };

                "Bing".metaData.hidden = true;
                "Google".metaData.hidden = true;
                "Wikipedia".metaData.alias = "@w";
              };
              force = true;
              default = "DuckDuckGo";
            };
        };
      };
    };
}
