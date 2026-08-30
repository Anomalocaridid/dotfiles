# Setup to run Kodi as a standalone appliance
{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.kodi =
    let
      # NOTE: Path relative to ~kodi
      kodiDownloadsDir = "Downloads";
    in
    {
      nixos =
        {
          config,
          lib,
          pkgs,
          ...
        }:
        {
          environment.persistence.${persistDir}.users.kodi.directories = [
            # Has settings and other necessary state
            ".kodi/userdata"

            # Has Elementum torrents and other necessary state
            ".kodi/temp"

            # Downloaded media
            kodiDownloadsDir
          ];

          users = {
            users.kodi = {
              group = "kodi";
              # Allow kodi user access to keyboards
              extraGroups = [ "input" ];
              isSystemUser = true;
              home = "/var/lib/kodi";
              createHome = true;
            };

            groups.kodi = { };
          };

          services.greetd = {
            enable = true;
            settings = {
              initial_session =
                let
                  customKodi =
                    (pkgs.kodi-gbm.overrideAttrs (oldAttrs: {
                      # Suppress popup on first boot that prompt to enable addons installed with nix
                      cmakeFlags = (oldAttrs.cmakeFlags or [ ]) ++ [ "-DADDONS_CONFIGURE_AT_STARTUP=OFF" ];
                    })).withPackages
                      (kodiPkgs: [
                        # Elementum add-on
                        (kodiPkgs.callPackage ./_pkgs/elementum.nix { })
                        # Elementum Burst provider
                        (kodiPkgs.callPackage ./_pkgs/elementum-burst.nix { })
                        # Elementum context menu
                        (kodiPkgs.callPackage ./_pkgs/elementum-context.nix { })
                        # Subtitle provider
                        kodiPkgs.a4ksubtitles
                      ]);
                in
                {
                  command = lib.getExe (
                    pkgs.writeShellApplication {
                      name = "kodi-wrapper.sh";

                      runtimeInputs = with pkgs; [
                        customKodi
                        wait4x
                        websocat
                      ];

                      text =
                        let
                          enableAddonJSON =
                            addonid:
                            builtins.toJSON {
                              jsonrpc = "2.0";
                              id = "startup";
                              method = "Addons.SetAddonEnabled";
                              params = {
                                inherit addonid;
                                enabled = true;
                              };
                            };
                        in
                        # shell
                        ''
                          # Run Kodi in background
                          kodi-standalone &

                          readonly kodi_process_id="$!"

                          # Wait for Kodi's local JSON-RPC port to open
                          wait4x --quiet tcp 127.0.0.1:9090

                          # Ensure add-ons are enabled
                          websocat --unidirectional ws://127.0.0.1:9090 << EOF
                          ${enableAddonJSON "plugin.video.elementum"}
                          ${enableAddonJSON "script.elementum.burst"}
                          ${enableAddonJSON "context.elementum"}
                          ${enableAddonJSON "service.subtitles.a4ksubtitles"}
                          EOF

                          # Wait for Kodi to exit so the session does not terminate prematurely
                          wait "$kodi_process_id"
                        '';
                    }
                  );
                  user = "kodi";
                };

              # Add method to access a tty to prevent being locked out if something breaks
              default_session.command = "${lib.getExe pkgs.tuigreet} --cmd ${
                lib.getExe config.users.users.${username}.shell
              }";
            };
          };
        };

      home =
        {
          config,
          lib,
          pkgs,
          osConfig,
          ...
        }:
        {
          programs.kodi = {
            enable = true;
            settings = {
              services = {
                # Set device's name in Kodi
                devicename = osConfig.networking.hostName;

                # Ensure that TCP port used for JSON-RPC API is on
                esenabled = "true";
                # ...but not open to the network
                esallinterfaces = "false";

                # Turn off unused network services
                zeroconf = "false";
                upnp = "false";
                airplay = "false";
                webserver = "false";
              };

              # Turn off display after 10 minutes
              powermanagement.displaysoff = "10";

              # Never check for addon updates to minimize config drift
              general.addonupdates = "2";

              # Ensure that addons from unknown sources cannot be installed to minimize config drift
              addons.unknownsources = "false";

              # Subtitle settings
              subtitles = rec {
                languages = "English";

                # Set default subtitle provider
                tv = "services.subtitles.a4ksubtitles";
                movie = tv;
              };
            };

            # Fake Elementum config, needed for hack to let Elementum config be mutable
            addonSettings."hm_plugin.video.elementum" = {
              # Do not bother searching for elementum repository addon
              skip_repository_search = "true";

              # Download files to disk rather than to RAM
              download_path = "${config.home.homeDirectory}/${kodiDownloadsDir}/";
              download_storage = "0";
              download_file_strategy = "2";

              # Continuing downloading after play is stopped
              keep_downloading = "0";
              # Keep files for not-finished videos
              keep_files_playing = "0";
              # Keep files for watched videos
              keep_files_finished = "0";

              # Do not move completed downloads
              # Set by default, but fixed just in case
              completed_move = "false";

              # Do not automatically respond with yes after timeout
              autoyes_enabled = "false";

              # Default path, but fix them just in case
              torrents_path = "special://temp/elementum_torrents/";

              # Disable download status overlay during playback when paused
              enable_overlay_status = "false";
            };
          };

          home = {
            # Hack to allow for certain settings to be mutable since some functionality relies on logging into accounts
            activation.elementumConfig =
              let
                addonDataDir = "${config.programs.kodi.datadir}/userdata/addon_data";
                elementumSettingsFile = lib.escapeShellArg "${addonDataDir}/plugin.video.elementum/settings.xml";
                hmElementumSettingsFile = lib.escapeShellArg "${addonDataDir}/hm_plugin.video.elementum/settings.xml";
              in
              lib.hm.dag.entryAfter [ "linkGeneration" ] # sh
                ''
                  # Ensure Elementum settings file exists; a blank file is acceptable
                  mkdir --parents ${dirOf elementumSettingsFile}
                  run touch ${elementumSettingsFile}

                  # Merge HM-generated Elementum config with mutable Elementum config
                  # Any settings set for HM-generated config must overwrite the same settings mutable config and leave others untouched
                  # Also make sure to save the `version` attribute of `<settings>` or things will break
                  # Adapted from code at https://mikefarah.gitbook.io/yq/operators/multiply-merge#merge-arrays-of-objects-together-matching-on-a-key
                  run ${lib.getExe pkgs.yq-go} --inplace eval-all '
                  	(select(fileIndex == 1) | .settings.+@version) as $version
                      | (((.settings.setting + .settings.setting  | .[] | {.+@id:  .}) as $item ireduce({}; . * $item )) as $uniqueMap
                        | ($uniqueMap  | to_entries | .[]) as $item ireduce([]; . + $item.value)) as $mergedArray
                      | select(fileIndex == 0)
                      | .settings.setting = $mergedArray
                      | .settings.+@version = $version
                  ' ${elementumSettingsFile} ${hmElementumSettingsFile}
                '';

            # Workaround because the default Estuary skin needs the type of each setting set explictly for some reason
            file.".kodi/userdata/addon_data/skin.estuary/settings.xml" = {
              force = true;
              text =
                let
                  makeSetting =
                    id: type: value: # xml
                    ''<setting id="${id}" type="${type}">${value}</setting>'';
                in
                # xml
                ''
                  <settings>
                    <!-- Only leave Add-Ons button -->
                    ${makeSetting "homemenunomoviebutton" "bool" "true"}
                    ${makeSetting "homemenunotvshowbutton" "bool" "true"}
                    ${makeSetting "homemenunomusicbutton" "bool" "true"}
                    ${makeSetting "homemenunomusicvideobutton" "bool" "true"}
                    <!-- NOTE: This is for live TV, not TV shows -->
                    ${makeSetting "homemenunotvbutton" "bool" "true"}
                    ${makeSetting "homemenunoradiobutton" "bool" "true"}
                    ${makeSetting "homemenunogamesbutton" "bool" "true"}
                    ${makeSetting "homemenunopicturesbutton" "bool" "true"}
                    ${makeSetting "homemenunovideosbutton" "bool" "true"}
                    ${makeSetting "homemenunofavbutton" "bool" "true"}
                    ${makeSetting "homemenunoweatherbutton" "bool" "true"}

                    <!-- Set background pattern -->
                    ${makeSetting "background_overlay" "string" "4"}
                  </settings>
                '';
            };
          };
        };
    };
}
