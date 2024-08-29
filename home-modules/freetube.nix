{ config, lib, ... }:
{
  programs.freetube = {
    enable = true;
    settings =
      let
        mkUpper =
          str:
          (lib.toUpper (builtins.substring 0 1 str)) + (builtins.substring 1 (builtins.stringLength str) str);
        # NOTE: baseTheme does not capitalize first letter, but other theme settings do
        flavor = "catppuccin${mkUpper config.catppuccin.flavor}";
        accent = mkUpper "${flavor}${mkUpper config.catppuccin.accent}";
      in
      {
        # General Settings
        checkForUpdates = false;
        checkForBlogPosts = false;

        # Theme Settings
        baseTheme = flavor;
        mainColor = accent;
        secColor = accent;

        # Distraction Free Settings
        ## Side Bar
        hideTrendingVideos = true;
        hidePopularVideos = true;

        ## Subscriptions Page
        hideSubscriptionsLive = true;
        hideSubscriptionsShorts = true;
        hideSubscriptionsCommunity = true;

        ## Watch Page
        hideVideoLikesAndDislikes = true;
        hideLiveChat = true;
        hideRecommendedVideos = true;
        hideComments = true;

        ## General
        hideVideoVies = true;
        hideChannelSubscriptions = true;

        # Download Settings
        downloadBehavior = "download";

        # SponsorBlock Settings
        useSponsorBlock = true;
        useDeArrowTitles = true;
        useDeArrowThumbnails = true;
      };
  };
}
