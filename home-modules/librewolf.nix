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

      # Generate file for locked settings
      mkAutoconfigJs =
        prefs:
        lib.concatStrings (
          lib.mapAttrsToList (name: value: ''
            lockPref("${name}", ${builtins.toJSON value}); 
          '') prefs
        );
    in
    {
      enable = true;
      nativeMessagingHosts = with pkgs; [
        keepassxc
      ];
      package = pkgs.librewolf.override {
        # Set preferences here to ensure they stay set
        extraPrefs = mkAutoconfigJs {
          # Only sync bookmarks and tabs
          # Some of these are set by default, but they should be locked just in case
          "services.sync.engine.bookmarks" = true;
          "services.sync.engine.tabs" = true;
          "services.sync.engine.addons" = false;
          "services.sync.engine.addresses" = false;
          "services.sync.engine.addresses.available" = false;
          "services.sync.engine.creditcards" = false;
          "services.sync.engine.creditcards.available" = false;
          "services.sync.engine.history" = false;
          "services.sync.engine.passwords" = false;
          "services.sync.engine.prefs" = false;
          # Enable sync
          "identity.fxaccounts.enabled" = true;
          # Automatically enable installed extensions
          "extensions.autoDisableScopes" = 0;
          # Set theme
          "extensions.activeThemeID" = "${catppuccin-firefox.addonId}";
          # Use same search engine for private browsing
          "browser.search.separatePrivateDefault" = false;
          # Clear history on shutdown
          # This should be on by default but is not for some reason
          "privacy.clearOnShutdown.history" = true;
          "privacy.clearOnShutdown_v2.historyFormDataAndDownloads" = true;
          # Customize toolbar
          # NOTE: needs to be set as a string here so it is formatted properly in the config
          "browser.uiCustomization.state" = builtins.toJSON {
            placements = {
              # Overflow dropdown
              widget-overflow-fixed-list = [ ];
              # Extensions dropdown
              unified-extensions-area = [
                "redirector_einaregilsson_com-browser-action"
                "_bbb880ce-43c9-47ae-b746-c3e0096c5b76_-browser-action"
                "_ublacklist-browser-action"
                "addon_darkreader_org-browser-action"
                "_7a7a4a92-a2a0-41d1-9fd7-1e92480d612d_-browser-action"
                "_cb31ec5d-c49a-4e5a-b240-16c767444f62_-browser-action"
              ];
              # Toolbar
              nav-bar = [
                "back-button"
                "forward-button"
                "stop-reload-button"
                "sidebar-button"
                "customizableui-special-spring1"
                "vertical-spacer"
                "urlbar-container"
                "customizableui-special-spring2"
                "screenshot-button"
                "downloads-button"
                "fxa-toolbar-menu-button"
                "ublock0_raymondhill_net-browser-action"
                "keepassxc-browser_keepassxc_org-browser-action"
                "unified-extensions-button"
              ];
              toolbar-menubar = [
                "menubar-items"
              ];
              TabsToolbar = [
                "firefox-view-button"
                "tabbrowser-tabs"
                "new-tab-button"
                "alltabs-button"
              ];
              vertical-tabs = [ ];
              PersonalToolbar = [
                "personal-bookmarks"
              ];
            };
            seen = [
              "redirector_einaregilsson_com-browser-action"
              "_bbb880ce-43c9-47ae-b746-c3e0096c5b76_-browser-action"
              "_ublacklist-browser-action"
              "addon_darkreader_org-browser-action"
              "keepassxc-browser_keepassxc_org-browser-action"
              "ublock0_raymondhill_net-browser-action"
              "_7a7a4a92-a2a0-41d1-9fd7-1e92480d612d_-browser-action"
              "_cb31ec5d-c49a-4e5a-b240-16c767444f62_-browser-action"
              "developer-button"
            ];
            dirtyAreaCache = [
              "unified-extensions-area"
              "nav-bar"
              "vertical-tabs"
            ];
            currentVersion = 21;
            newElementCount = 4;
          };
        };
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
            ublock-origin
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
              catppuccin-web-file-icons
              darkreader
              indie-wiki-buddy
              keepassxc-browser # NOTE: needs mutable settings
              redirector
              stylus
              ublacklist
              ublock-origin # NOTE: use policies instead if this needs to be configured
            ];
            settings =
              let
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
                      fontFamily = builtins.head config.fonts.fontconfig.defaultFonts.monospace;
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
                  icon = "https://search.nixos.org/favicon.png";
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
                  icon = "https://wiki.nixos.org/favicon.ico";
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
                  icon = "https://home-manager-options.extranix.com/images/favicon.png";
                  updateInterval = 24 * 60 * 60 * 1000; # every day
                  definedAliases = [ "@nhm" ];
                };

                bing.metaData.hidden = true;
                google.metaData.hidden = true;
                wikipedia.metaData.alias = "@w";
              };
              force = true;
              default = "ddg";
            };
        };
      };
    };
}
