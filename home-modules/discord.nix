{
  config,
  lib,
  inputs,
  ...
}:
{
  imports = [ inputs.nixcord.homeModules.nixcord ];

  programs.nixcord = {
    enable = true;
    discord.enable = false;
    vesktop = {
      enable = true;
      settings =
        let
          palette =
            (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
            .${config.catppuccin.flavor}.colors;
          rgb =
            {
              r,
              g,
              b,
            }:
            "rgb(${toString r}, ${toString g}, ${toString b})";
        in
        {
          discordBranch = "stable";
          staticTitle = true;
          splashTheming = true;
          splashColor = rgb palette.text.rgb;
          splashBackground = rgb palette.base.rgb;
          enableSplashScreen = false;
          minimizeToTray = true;
          clickTrayToShowHide = true;
          arRPC = true;
        };
      state = {
        firstLaunch = false;
      };
    };
    config = {
      themeLinks = [
        "https://raw.githubusercontent.com/catppuccin/discord/refs/heads/main/themes/${config.catppuccin.flavor}.theme.css"
      ];
      plugins = {
        alwaysAnimate.enable = true;
        anonymiseFileNames = {
          enable = true;
          anonymiseByDefault = true;
        };
        betterFolders = {
          enable = true;
          sidebar = false;
          closeAllFolders = true;
          closeAllHomeButton = true;
          closeOthers = true;
          forceOpen = true;
        };
        betterGifAltText.enable = true;
        betterGifPicker.enable = true;
        biggerStreamPreview.enable = true;
        blurNSFW.enable = true;
        callTimer = {
          enable = true;
          format = "human";
        };
        clearURLs.enable = true;
        fakeNitro.enable = true;
        friendsSince.enable = true;
        fullSearchContext.enable = true;
        greetStickerPicker.enable = true;
        implicitRelationships.enable = true;
        mentionAvatars.enable = true;
        moreKaomoji.enable = true;
        mutualGroupDMs.enable = true;
        noF1.enable = true;
        silentTyping.enable = true;
        typingIndicator.enable = true;
        typingTweaks.enable = true;
        viewIcons.enable = true;
        viewRaw.enable = true;
        volumeBooster.enable = true;
        whoReacted.enable = true;
        youtubeAdblock.enable = true;
      };
    };
  };
}
