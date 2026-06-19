{ config, ... }:
{
  unify.modules.desktop = {
    nixos =
      { pkgs, ... }:
      let
        inherit (config.flake.meta) username persistDir;
      in
      {
        # Needed for certain mods like VinURL
        programs.nix-ld.enable = true;
        # Persist Prism Launcher data
        environment.persistence.${persistDir}.users.${username}.directories = [
          ".local/share/PrismLauncher"
        ];
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
        # Install Minecraft modpack creator
        home.packages = with pkgs; [ packwiz ];

        programs.prismlauncher =
          let
            graalvm-ce-21 = pkgs.graalvmPackages.buildGraalvm rec {
              version = "21.0.2";
              src = pkgs.fetchurl {
                url = "https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-${version}/graalvm-community-jdk-${version}_linux-x64_bin.tar.gz";
                hash = "sha256-sEgGmqo6mbhPW5V7FizBgaMqQzDLw1QCdmNjxb52rkg=";
              };
            };
          in
          {
            enable = true;

            package = pkgs.prismlauncher.override {
              # Needed for certain mods like VinURL
              additionalPrograms = with pkgs; [ ffmpeg ];

              jdks = with pkgs.graalvmPackages; [
                # JDK 25
                graalvm-ce
                # JDK 21
                graalvm-ce-21
              ];
            };

            settings = {
              # Set default java path
              JavaPath = lib.getExe graalvm-ce-21;
              ApplicationTheme = "system"; # Use system Qt theme
              AutomaticJavaDownload = false;
              AutomaticJavaSwitch = true;
              IconTheme = "custom";
              # Needed to prevent Language wizard from showing up
              Language = osConfig.i18n.defaultLocale;
              MaxMemAlloc = "12288"; # 12 GiB
              MinMemAlloc = "512"; # 512 MiB
              UserAskedAboutAutomaticJavaDownload = true;
              EnableFeralGamemode = osConfig.programs.gamemode.enable;
              JvmArgs = lib.concatStringsSep " " [
                # Enable Discord rich presence
                "-DAllowMcDiscordDetection=net.minecraft.client.main.Main"
                # Optimization flags from https://github.com/brucethemoose/Minecraft-Performance-Flags-Benchmarks
                # NOTE: these assume Java 11+
                ## Base Java Flags
                "-XX:+UnlockExperimentalVMOptions"
                "-XX:+UnlockDiagnosticVMOptions"
                "-XX:+AlwaysActAsServerClassMachine"
                "-XX:+AlwaysPreTouch"
                "-XX:+DisableExplicitGC"
                "-XX:+UseNUMA"
                "-XX:NmethodSweepActivity=1"
                "-XX:ReservedCodeCacheSize=400M"
                "-XX:NonNMethodCodeHeapSize=12M"
                "-XX:ProfiledCodeHeapSize=194M"
                "-XX:NonProfiledCodeHeapSize=194M"
                "-XX:-DontCompileHugeMethods"
                "-XX:MaxNodeLimit=240000"
                "-XX:NodeLimitFudgeFactor=8000"
                "-XX:+UseVectorCmov"
                "-XX:+PerfDisableSharedMem"
                "-XX:+UseFastUnorderedTimeStamps"
                "-XX:+UseCriticalJavaThreadPriority"
                "-XX:ThreadPriorityPolicy=1"
                "-XX:AllocatePrefetchStyle=3"
                ## Garbage Collection (Client G1GC, which is only option for GraalVM)
                "-XX:+UseG1GC"
                "-XX:MaxGCPauseMillis=37"
                "-XX:+PerfDisableSharedMem"
                "-XX:G1HeapRegionSize=16M"
                "-XX:G1NewSizePercent=23"
                "-XX:G1ReservePercent=20"
                "-XX:SurvivorRatio=32"
                "-XX:G1MixedGCCountTarget=3"
                "-XX:G1HeapWastePercent=20"
                "-XX:InitiatingHeapOccupancyPercent=10"
                "-XX:G1RSetUpdatingPauseTimePercent=0"
                "-XX:MaxTenuringThreshold=1"
                "-XX:G1SATBBufferEnqueueingThresholdPercent=30"
                "-XX:G1ConcMarkStepDurationMillis=5.0"
                "-XX:G1ConcRSHotCardLimit=16"
                "-XX:G1ConcRefinementServiceIntervalMillis=150"
                "-XX:GCTimeRatio=99"
              ];
            };
          };

        xdg.dataFile."PrismLauncher/iconthemes/custom".source =
          pkgs.runCommand "prismlauncher-icons"
            {
              ICONS = "${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}";
            }
            ''
              mkdir --parents $out/scalable

              cat << EOF > $out/index.theme
              [Icon Theme]
              Name=custom
              Comment=Custom icon theme
              Inherits=multimc
              Directories=scalable

              [scalable]
              Size=96
              Type=Scalable
              MinSize=8
              MaxSize=256
              EOF

              # Alphabetical order by destination
              cp $ICONS/apps/scalable/help-about.svg $out/scalable/about.svg
              cp $ICONS/apps/scalable/system-users.svg $out/scalable/accounts.svg
              cp $ICONS/apps/scalable/atlauncher.svg $out/scalable/atlauncher.svg
              cp $ICONS/apps/scalable/tools-report-bug.svg $out/scalable/bug.svg
              cp $ICONS/places/48/folder-favorites.svg $out/scalable/centralmods.svg
              cp $ICONS/apps/scalable/system-software-update.svg $out/scalable/checkupdate.svg
              cp $ICONS/places/16/edit-copy.svg $out/scalable/copy.svg
              cp $ICONS/apps/scalable/terminal.svg $out/scalable/custom-commands.svg
              cp $ICONS/places/48/user-trash.svg $out/scalable/delete.svg
              cp $ICONS/apps/scalable/discord.svg $out/scalable/discord.svg
              cp $ICONS/apps/scalable/terminal.svg $out/scalable/environment-variables.svg
              cp $ICONS/places/48/folder-download.svg $out/scalable/export.svg
              cp $ICONS/apps/scalable/terminal.svg $out/scalable/externaltools.svg
              cp $ICONS/apps/scalable/help-browser.svg $out/scalable/help.svg
              cp $ICONS/apps/scalable/preferences-system.svg $out/scalable/instance-settings.svg
              cp $ICONS/apps/scalable/java.svg $out/scalable/jarmods.svg
              cp $ICONS/apps/scalable/java.svg $out/scalable/java.svg
              cp $ICONS/apps/scalable/preferences-desktop-locale.svg $out/scalable/language.svg
              cp $ICONS/apps/scalable/preferences-desktop-multimedia.svg $out/scalable/launch.svg # not working?
              cp $ICONS/apps/scalable/prism-launcher.svg $out/scalable/launcher.svg
              cp $ICONS/mimetypes/scalable/package-x-generic.svg $out/scalable/loadermods.svg
              cp $ICONS/mimetypes/scalable/text-x-generic.svg $out/scalable/log.svg
              cp $ICONS/apps/scalable/irc-chat.svg $out/scalable/matrix.svg
              cp $ICONS/apps/scalable/minecraft.svg $out/scalable/minecraft.svg
              # $out/scalable/mojang.svg
              cp $ICONS/places/16/folder-new.svg $out/scalable/new.svg
              cp $ICONS/apps/scalable/internet-news-reader.svg $out/scalable/news.svg
              cp $ICONS/apps/scalable/accessories-notes.svg $out/scalable/notes.svg
              # $out/scalable/patreon.svg
              cp $ICONS/preferences/scalable/proxy.svg $out/scalable/proxy.svg
              # $out/scalable/reddit-alien.svg
              cp $ICONS/apps/scalable/system-reboot.svg $out/scalable/refresh.svg
              cp $ICONS/apps/scalable/accessories-text-editor.svg $out/scalable/rename.svg
              cp $ICONS/apps/scalable/applications-other.svg $out/scalable/resourcepacks.svg
              cp $ICONS/apps/scalable/applets-screenshooter.svg $out/scalable/screenshots.svg
              cp $ICONS/places/16/server.svg $out/scalable/server.svg
              cp $ICONS/apps/scalable/preferences-system.svg $out/scalable/settings.svg
              cp $ICONS/apps/scalable/applications-other.svg $out/scalable/shaderpacks.svg
              cp $ICONS/apps/scalable/preferences-web-browser-shortcuts.svg $out/scalable/shortcut.svg
              # $out/scalable/status-bad.svg # TODO: when icon pack has emblems
              # $out/scalable/status-good.svg # TODO: when icon pack has emblems
              # $out/scalable/status-running.svg # TODO: when icon pack has emblems
              # $out/scalable/status-yellow.svg # TODO: when icon pack has emblems
              cp $ICONS/apps/scalable/easytag.svg $out/scalable/tag.svg
              # $out/scalable/technic.svg
              cp $ICONS/places/48/folder.svg $out/scalable/viewfolder.svg
              cp $ICONS/apps/scalable/cs-network.svg $out/scalable/worlds.svg
            '';
      };
  };
}
