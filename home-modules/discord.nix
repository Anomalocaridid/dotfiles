{ pkgs, config, ... }:
{
  programs.nixcord = {
    enable = true;
    discord.enable = false;
    vesktop = {
      enable = true;
      # TODO: remove when nixpkgs#368221 is merged
      package = pkgs.vesktop.overrideAttrs (oldAttrs: {
        patches = (oldAttrs.patches or [ ]) ++ [ ../assets/readonlyFix.patch ];
      });
      settings = {
        discordBranch = "stable";
        staticTitle = true;
        splashTheming = true;
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
