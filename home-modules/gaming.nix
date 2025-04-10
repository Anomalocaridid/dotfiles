{
  config,
  pkgs,
  inputs,
  ...
}:
{
  home.packages = with pkgs; [
    gamescope # Used by Lutris for control over game resolution
    lutris
    packwiz # minecraft modpack creator
    prismlauncher
    # TODO: uncomment when package no longer uses python311Packages rather than individual packages
    # pysolfc
    runelite
  ];

  # Enable wine-ge's fsync support
  home.sessionVariables.WINEFSYNC = 1;

  xdg.dataFile = {
    "PrismLauncher/themes/catppuccin".source =
      inputs.catppuccin-prismlauncher + /themes/${config.catppuccin.flavor}/${config.catppuccin.accent};
    "PrismLauncher/iconthemes/custom".source =
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
}
