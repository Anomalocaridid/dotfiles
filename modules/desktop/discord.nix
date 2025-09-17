{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.general = {
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [
      ".config/vesktop/sessionData" # Vesktop user data
    ];

    home =
      { config, ... }:
      {
        programs.vesktop = {
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
            };
          vencord = {
            useSystem = true;
            settings = {
              plugins = {
                AlwaysAnimate.enabled = true;
                AnonymiseFileNames = {
                  enabled = true;
                  anonymiseByDefault = true;
                };
                BetterFolders = {
                  enabled = true;
                  sidebar = false;
                  closeAllFolders = true;
                  closeAllHomeButton = true;
                  closeOthers = true;
                  forceOpen = true;
                };
                BetterGifAltText.enabled = true;
                BetterGifPicker.enabled = true;
                BiggerStreamPreview.enabled = true;
                BlurNSFW.enabled = true;
                CallTimer = {
                  enabled = true;
                  format = "human";
                };
                ClearURLs.enabled = true;
                FakeNitro.enabled = true;
                FriendsSince.enabled = true;
                FullSearchContext.enabled = true;
                GreetStickerPicker.enabled = true;
                ImplicitRelationships.enabled = true;
                MentionAvatars.enabled = true;
                MoreKaomoji.enabled = true;
                MutualGroupDMs.enabled = true;
                NoF1.enabled = true;
                NoReplyMention = {
                  enabled = true;
                  inverseShiftReply = true;
                };
                SilentTyping.enabled = true;
                TypingIndicator.enabled = true;
                ViewIcons.enabled = true;
                ViewRaw.enabled = true;
                VolumeBooster.enabled = true;
                WhoReacted.enabled = true;
                YoutubeAdblock.enabled = true;
              };
            };
          };
        };

        # Prevent Vesktop from showing first-launch config dialogue
        xdg.configFile."vesktop/state.json".text = builtins.toJSON {
          firstLaunch = false;
        };
      };
  };
}
