{ config, inputs, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  flake-file.inputs.nixcord = {
    url = "github:kaylorben/nixcord";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-parts.follows = "flake-parts";
    };
  };

  unify.modules.general = {
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [
      ".config/equibop/sessionData" # Equibop user data
    ];

    home =
      { config, pkgs, ... }:
      let
        themeFile = "catppuccin.theme.css";
      in
      {
        imports = [ inputs.nixcord.homeModules.nixcord ];

        # Add theme file
        xdg.configFile."equibop/themes/${themeFile}".text = # css
          let
            inherit (config.catppuccin) flavor accent;
          in
          # css
          ''
            /**
             * @name Catppuccin ${flavor} (${accent})
             * @author Catppuccin
             * @description 🎮 Soothing pastel theme for Discord
             * @website https://github.com/catppuccin/discord
            **/
            @import url("https://catppuccin.github.io/discord/dist/catppuccin-${flavor}-${accent}.theme.css");
          '';

        programs.nixcord = {
          enable = true;
          discord.enable = false;
          equibop = {
            enable = true;
            settings =
              let
                palette = config.catppuccin.sources.parsedPalette;
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
                # Minimize flashing on startup
                splashBackground = rgb palette.mantle.rgb;
                enableSplashScreen = false;
                minimizeToTray = true;
                clickTrayToShowHide = true;
                arRPC = true;
                splashProgess = true;
              };
            state = {
              firstLaunch = false;
            };
          };
          equibopConfig = {
            enabledThemes = [ themeFile ];
            plugins = {
              alwaysAnimate.enable = true;
              amITyping.enable = true;
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
              betterInvites.enable = true;
              biggerStreamPreview.enable = true;
              blurNSFW.enable = true;
              callTimer = {
                enable = true;
                format = "human";
              };
              characterCounter.enable = true;
              clearURLs.enable = true;
              fakeNitro.enable = true;
              friendsSince.enable = true;
              fullSearchContext.enable = true;
              greetStickerPicker.enable = true;
              homeTyping.enable = true;
              implicitRelationships.enable = true;
              mentionAvatars.enable = true;
              moreKaomoji.enable = true;
              mutualGroupDMs.enable = true;
              noF1.enable = true;
              noNitroUpsell.enable = true;
              noReplyMention = {
                enable = true;
                inverseShiftReply = true;
              };
              previewMessage.enable = true;
              silentTyping.enable = true;
              typingIndicator.enable = true;
              typingtweaks.enable = true;
              viewIcons.enable = true;
              viewRaw.enable = true;
              volumeBooster.enable = true;
              whoReacted.enable = true;
              whosWatching.enable = true;
              youtubeAdblock.enable = true;
            };
          };
        };
      };
  };
}
