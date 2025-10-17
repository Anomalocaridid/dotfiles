{ config, inputs, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  # Catppuccin userstyles for Stylus
  flake-file.inputs = {
    nur = {
      url = "github:nix-community/NUR";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };

    catppuccin-userstyles-nix = {
      url = "github:different-name/catppuccin-userstyles-nix?rev=b347a087e34ddb4ce645014744b101f217350209";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  unify.modules.general = {
    # Librewolf data
    nixos = {
      # Nix user repository
      nixpkgs.overlays = [ inputs.nur.overlays.default ];

      environment.persistence.${persistDir}.users.${username}.directories = [ ".librewolf" ];
    };

    home =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        # Force Firefox Color settings
        catppuccin.librewolf.force = true;

        xdg.mimeApps.defaultApplications =
          let
            desktopFile = "librewolf.desktop";
          in
          {
            "text/html" = desktopFile;
            "x-scheme-handler/http*" = desktopFile;
          };

        programs.librewolf =
          let
            inherit (pkgs.nur.repos.rycee) firefox-addons;
            # Generate file for locked settings
            mkAutoconfigJs =
              prefs:
              lib.concatStrings (
                lib.mapAttrsToListRecursive (path: value: ''
                  lockPref("${lib.concatStringsSep "." path}", ${builtins.toJSON value}); 
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

                # Enable sync
                identity.fxaccounts.enabled = true;

                # Automatically enable installed extensions
                extensions.autoDisableScopes = 0;

                privacy = {
                  # Clear history on shutdown
                  # This should be on by default but is not for some reason
                  clearOnShutdown.history = true;

                  # Some of these are set by default, but lock them just in case
                  clearOnShutdown_v2 = {
                    browsingHistoryAndDownloads = true;
                    cache = true;
                    cookiesAndStorage = true;
                    formdata = true;
                    historyFormDataAndDownloads = true;
                    siteSettings = true;
                  };

                  # Enable letterboxing to resist fingerprinting
                  resistFingerprinting.letterboxing = true;
                };

                browser = {
                  # Use same search engine for private browsing
                  search.separatePrivateDefault = false;

                  # Customize toolbar
                  # NOTE: needs to be set as a string here so it is formatted properly in the config
                  uiCustomization.state = builtins.toJSON {
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
            };

            profiles = {
              # Separate profile to enable WebGL because it allows more fingerprinting
              gaming = {
                id = 1;
                # Un-break certain websites
                settings."webgl.disabled" = false;
                extensions = {
                  # Needed because of above settings
                  force = true;
                  packages = with firefox-addons; [
                    firefox-color # Required for catppuccin theme
                    canvasblocker # Recommended for enabled WebGL
                    keepassxc-browser
                    ublock-origin
                  ];
                };
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
                    firefox-color # Required for catppuccin theme
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
                search =
                  let
                    mkParams = lib.mapAttrsToList lib.nameValuePair;
                    # Update favicons every day
                    updateInterval = 24 * 60 * 60 * 1000;
                  in
                  {
                    engines = {
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
                        definedAliases = [ "@np" ];
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
                        definedAliases = [ "@no" ];
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
                        definedAliases = [ "@nhm" ];
                      };

                      "NixOS Wiki" = {
                        inherit updateInterval;
                        urls = [
                          {
                            template = "https://wiki.nixos.org/index.php";
                            params = mkParams {
                              search = "{searchTerms}";
                            };
                          }
                        ];
                        icon = "https://wiki.nixos.org/favicon.ico";
                        definedAliases = [ "@nw" ];
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
      };
  };
}
