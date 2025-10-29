{ config, inputs, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  flake-file.inputs = {
    # Catppuccin userstyles for Stylus
    catppuccin-userstyles-nix = {
      url = "github:different-name/catppuccin-userstyles-nix?rev=b347a087e34ddb4ce645014744b101f217350209";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Firefox userChrome
    cascade = {
      url = "github:cascadefox/cascade";
      flake = false;
    };
  };

  unify.modules.general = {
    # Librewolf data
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [ ".librewolf" ];

    home =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        homePage = "www.startpage.com";
      in
      {
        # Force Firefox Color settings
        catppuccin.librewolf.force = true;

        xdg = {
          mimeApps.defaultApplications =
            let
              desktopFile = "librewolf.desktop";
            in
            {
              "text/html" = desktopFile;
              "x-scheme-handler/http*" = desktopFile;
            };

          # Tridactyl config
          configFile."tridactyl/tridactylrc".text =
            let
              theme = "catppuccin-${config.catppuccin.flavor}";
            in
            ''
              " Clear commandline history and config not present in config file
              sanitize commandline tridactylsync tridactyllocal

              " Set colorscheme
              " TODO: change URL when repo is made official: https://github.com/catppuccin/catppuccin/issues/2799
              colourscheme --url https://raw.githubusercontent.com/devnullvoid/tridactyl/catppuccin-review-changes/themes/${theme}.css ${theme}

              " Set new tab page
              set newtab ${homePage}

              " Set text editor for editing text fields
              set editorcmd handlr launch text/plain --

              " Adjust keybinds to account for vertical tabs
              bind J tabnext
              bind K tabprev
            '';
        };

        programs.librewolf = {
          enable = true;
          nativeMessagingHosts = with pkgs; [
            keepassxc
            tridactyl-native
          ];
          package = pkgs.librewolf.override {
            # Set preferences not supported in Preferences policy here to ensure they stay set
            extraPrefs =
              let
                # Generate file for locked settings
                mkAutoconfigJs =
                  prefs:
                  lib.concatStrings (
                    lib.mapAttrsToListRecursive (path: value: ''
                      lockPref("${lib.concatStringsSep "." path}", ${builtins.toJSON value});
                    '') prefs
                  );
              in
              mkAutoconfigJs {
                # Enable sync
                # NOTE: DisableFirefoxAccounts policy cannot turn sync on if it is already off by default
                identity.fxaccounts.enabled = true;

                # Only sync bookmarks and tabs
                # Some of these are set by default, but they should be locked just in case
                services.sync.engine = {
                  bookmarks = true;
                  tabs = true;
                  addons = false;
                  addresses = false;
                  "addresses.available" = false;
                  creditcards = false;
                  "creditcards.available" = false;
                  history = false;
                  passwords = false;
                  prefs = false;
                };

                # Enable letterboxing to resist fingerprinting
                privacy.resistFingerprinting.letterboxing = true;

                sidebar = {
                  # New style sidebar required for vertical tabs
                  revamp = true;
                  # Enable vertical tabs
                  verticalTabs = true;
                  # Dismiss "Drag important tabs" window
                  "verticalTabs.dragToPinPromo".dismissed = true;
                  # Always show sidebar
                  visibility = "always-show";
                  # Set sidebar buttons
                  main.tools = "history,bookmarks";

                  # Set sidebar layout
                  # NOTE: needs to be set as a string
                  backupState = builtins.toJSON {
                    command = "";
                    panelOpen = false;
                    launcherWidth = 55;
                    launcherExpanded = false;
                    launcherVisible = true;
                  };
                };
              };
          };

          policies = {
            # Clear history and cookies on shutdown
            # This should be on by default but is not for some reason
            SanitizeOnShutdown = true;

            # Install extensions
            # Doing this via policies ensures they're activated and removed extensions are uninstalled
            ExtensionSettings =
              let
                mkExtensions =
                  ids:
                  lib.genAttrs ids (id: {
                    installation_mode = "force_installed";
                    install_url = "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";
                    private_browsing = true;
                  });
              in
              {
                # Block all other extensions
                "*".installation_mode = "blocked";
              }
              // (mkExtensions [
                # Firefox color
                "FirefoxColor@mozilla.com"
                # Catppuccin for GitHub File Explorer Icons
                "{bbb880ce-43c9-47ae-b746-c3e0096c5b76}"
                # Dark Reader
                "addon@darkreader.org"
                # Indie Wiki Buddy
                "{cb31ec5d-c49a-4e5a-b240-16c767444f62}"
                # KeepassXC Browser
                "keepassxc-browser@keepassxc.org"
                # Redirector
                "redirector@einaregilsson.com"
                # Stylus
                "{7a7a4a92-a2a0-41d1-9fd7-1e92480d612d}"
                # Tridactyl
                "tridactyl.vim@cmcaine.co.uk"
                # uBlacklist
                "@ublacklist"
                # uBlock Origin
                "uBlock0@raymondhill.net"
              ]);

            Preferences =
              let
                mkPreferences =
                  prefs:
                  lib.mergeAttrsList
                  <| lib.mapAttrsToListRecursive (path: value: {
                    "${lib.concatStringsSep "." path}" = {
                      Value = value;
                      Status = "locked";
                    };
                  }) prefs;
              in
              mkPreferences {
                # Enable userChrome
                toolkit.legacyUserProfileCustomizations.stylesheets = true;

                # Disable Alt key menu (somehow breaks cascade userChrome with persistent effects)
                ui.key.menuAccessKeyFocuses = false;

                browser = {
                  # Set home page so Tridactyl can run on it
                  startup.homepage = homePage;

                  # Use same search engine for private browsing
                  search.separatePrivateDefault = false;

                  # Hide download button when download history is empty
                  download.autohideButton = true;

                  # Customize toolbar
                  # NOTE: needs to be set as a string
                  uiCustomization.state = builtins.toJSON {
                    placements = {
                      widget-overflow-fixed-list = [ ];
                      unified-extensions-area = [
                        "redirector_einaregilsson_com-browser-action"
                        "_bbb880ce-43c9-47ae-b746-c3e0096c5b76_-browser-action"
                        "_ublacklist-browser-action"
                        "addon_darkreader_org-browser-action"
                        "_7a7a4a92-a2a0-41d1-9fd7-1e92480d612d_-browser-action"
                        "_cb31ec5d-c49a-4e5a-b240-16c767444f62_-browser-action"
                        "firefoxcolor_mozilla_com-browser-action"
                      ];
                      nav-bar = [
                        "back-button"
                        "forward-button"
                        "vertical-spacer"
                        "urlbar-container"
                        "downloads-button"
                        "fxa-toolbar-menu-button"
                        "ublock0_raymondhill_net-browser-action"
                        "keepassxc-browser_keepassxc_org-browser-action"
                        "unified-extensions-button"
                        "alltabs-button"
                      ];
                      toolbar-menubar = [ "menubar-items" ];
                      TabsToolbar = [ ];
                      vertical-tabs = [ "tabbrowser-tabs" ];
                      PersonalToolbar = [ "personal-bookmarks" ];
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
                      "firefoxcolor_mozilla_com-browser-action"
                      "screenshot-button"
                    ];
                    dirtyAreaCache = [
                      "unified-extensions-area"
                      "nav-bar"
                      "vertical-tabs"
                      "toolbar-menubar"
                      "TabsToolbar"
                      "PersonalToolbar"
                    ];
                    currentVersion = 23;
                    newElementCount = 5;
                  };
                };
              };
          };

          profiles.default = {
            # Needed to make config files
            preConfig = ''

            '';

            userChrome = # css
              ''
                /* Default settings */
                @import "${inputs.cascade}/chrome/includes/cascade-config.css";

                @import "${inputs.cascade}/chrome/includes/cascade-layout.css";
                @import "${inputs.cascade}/chrome/includes/cascade-responsive.css";
                @import "${inputs.cascade}/chrome/includes/cascade-floating-panel.css";

                @import "${inputs.cascade}/chrome/includes/cascade-nav-bar.css";
                @import "${inputs.cascade}/chrome/includes/cascade-tabs.css";

                /* Not default settings */
                @import "${inputs.cascade}/integrations/catppuccin/cascade-${config.catppuccin.flavor}.css";
              '';

            extensions = {
              # Override extension settings
              # NOTE: Each extension also needs `force = true` to prevent file conflicts
              force = true;
              settings =
                let
                  accent = config.catppuccin.accent;
                  palette = config.catppuccin.sources.parsedPalette;
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
                          description = "FreeTube";
                          exampleUrl = "https://www.youtube.com/watch?v=dQw4w9WgXcQ";
                          # Normally automatically generated, but will not be properly generated if missing
                          # Does not cause serious problems if missing, just mangles example in redirector list
                          exampleResult = "freetube://https://www.youtube.com/watch?v=dQw4w9WgXcQ";
                          includePattern = "((https://)?(www\\.)?youtu(be\\.com|\\.be)/.*)";
                          redirectUrl = "freetube://$1";
                          patternType = "R"; # Regular expression
                          # Required or redirector will not work
                          appliesTo = [
                            "main_frame"
                          ];
                        }
                        {
                          description = "Steam Client";
                          exampleUrl = "https://store.steampowered.com/";
                          # Normally automatically generated, but will not be properly generated if missing
                          # Does not cause serious problems if missing, just mangles example in redirector list
                          exampleResult = "steam://openurl/https://store.steampowered.com/";
                          includePattern = "^(https://(.*\\.)?steam(powered|community).com/)$";
                          redirectUrl = "steam://openurl/$1";
                          patternType = "R"; # Regular expression
                          appliesTo = [
                            "main_frame"
                          ];
                        }
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
                  "{7a7a4a92-a2a0-41d1-9fd7-1e92480d612d}" = {
                    force = true;
                    settings = inputs.catppuccin-userstyles-nix.stylusSettings.${pkgs.system} {
                      global = {
                        lightFlavor = config.catppuccin.flavor;
                        darkFlavor = config.catppuccin.flavor;
                        accentColor = config.catppuccin.accent;
                      };
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

            search = {
              force = true;
              # use built-in StartPage
              # id obtained with `mozlz4a -d ~/.librewolf/default/search.json.mozlz4`
              default = "policy-StartPage";

              # NOTE: For some reason, Tridactyl only detects aliases declared with `metaData.alias`, not `definedAliases`
              engines =
                let
                  mkParams = lib.mapAttrsToList lib.nameValuePair;
                  # Update favicons every day
                  updateInterval = 24 * 60 * 60 * 1000;
                in
                {
                  "NixOS Packages" = {
                    inherit updateInterval;
                    urls = [
                      {
                        template = "https://search.nixos.org/packages";
                        params = mkParams {
                          channel = "unstable";
                          query = "{searchTerms}";
                        };
                      }
                    ];
                    icon = "https://search.nixos.org/favicon.png";
                    metaData.alias = "@np";
                  };

                  "NixOS Options" = {
                    inherit updateInterval;
                    urls = [
                      {
                        template = "https://search.nixos.org/options";
                        params = mkParams {
                          channel = "unstable";
                          query = "{searchTerms}";
                        };
                      }
                    ];
                    icon = "https://search.nixos.org/favicon.png";
                    metaData.alias = "@no";
                  };

                  "Nix Home Manager" = {
                    inherit updateInterval;
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
                    metaData.alias = "@nhm";
                  };

                  "NixOS Wiki" = {
                    inherit updateInterval;
                    urls = [
                      {
                        template = "https://wiki.nixos.org/w/index.php";
                        params = mkParams {
                          search = "{searchTerms}";
                        };
                      }
                    ];
                    icon = "https://wiki.nixos.org/favicon.ico";
                    metaData.alias = "@nw";
                  };

                  bing.metaData.hidden = true;
                  google.metaData.hidden = true;
                  wikipedia.metaData.alias = "@w";
                };
            };
          };
        };
      };
  };
}
