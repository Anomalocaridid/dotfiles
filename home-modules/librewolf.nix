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
          extensions.packages = with firefox-addons; [
            canvasblocker # Recommended for enabled WebGL
            keepassxc-browser
          ];
        };
        default = {
          # Needed to make config files
          preConfig = ''

          '';
          extensions = {
            # Override extension settings
            # NOTE: Each extension also needs `force = true` to prevent file conflicts
            force = true;
            packages = with firefox-addons; [
              catppuccin-firefox
              catppuccin-gh-file-explorer
              darkreader
              indie-wiki-buddy
              keepassxc-browser # NOTE: needs mutable settings
              redirector
              stylus
              ublacklist
            ];
            settings =
              let
                fonts = config.stylix.fonts;
                accent = config.catppuccin.accent;
                palette =
                  (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
                  .${config.catppuccin.flavor}.colors;
              in
              {
                "addon@darkreader.org" = {
                  force = true;
                  settings = {
                    syncSettings = false;
                    theme = {
                      fontFamily = fonts.sansSerif.name;
                      darkSchemeBackgroundColor = palette.base.hex;
                      darkSchemeTextColor = palette.text.hex;
                      selectionColor = palette.surface2.hex;
                    };
                    previewNewDesign = true;
                  };
                };

                "redirector@einaregilsson.com" = {
                  force = true;
                  settings = {
                    redirects = [
                      {
                        description = "[Farside] General Entry";
                        exampleUrl = "https://m.youtube.com/watch?v=dQw4w9WgXcQ";
                        # Normally automatically generated, but will not be properly generated if missing
                        # Does not cause serious problems if missing, just mangles example in redirector list
                        exampleResult = "https://farside.link/youtube.com/watch?v=dQw4w9WgXcQ";
                        includePattern = "^(?:https?://)?(?:www\\.)?(?:\\w{2;}\\.)?(?:mobile\\.|m\\.)?((?:imdb|imgur|instagram|medium|odysee|quora|reddit|tiktok|translate\\.google|wikipedia|youtube)\\.(?:com|org|au|de|co|cn).*)$";
                        redirectUrl = "https://farside.link/$1";
                        patternType = "R"; # Regular expression
                        # Required or redirector will not work
                        appliesTo = [ "main_frame" ];
                      }
                    ];
                  };
                };

                # Stylus
                # TODO: figure out how to populate with Catppuccin userstyles
                "{7a7a4a92-a2a0-41d1-9fd7-1e92480d612d}" = {
                  force = true;
                  settings = {
                    dbInChromeStorage = true; # required for Stylus
                  };
                };

                "@ublacklist" = {
                  force = true;
                  settings = {
                    subscriptions = {
                      "0" = {
                        name = "Main AI blocklist";
                        url = "https://raw.githubusercontent.com/laylavish/uBlockOrigin-HUGE-AI-Blocklist/main/list_uBlacklist.txt";
                      };
                    };
                    updateInterval = 60;
                    linkColor = palette.blue.hex;
                    blockColor = palette.red.hex;
                    highlightColors = [ palette.${accent}.hex ];
                  };
                };
              };
          };
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
