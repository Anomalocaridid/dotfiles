{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  unify.modules.general = {
    # Freetube user data
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [ ".config/FreeTube" ];

    home.programs.freetube = {
      enable = true;
      settings = {
        # General Settings
        checkForUpdates = false;
        checkForBlogPosts = false;

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
        hideVideoViews = true;
        hideChannelSubscriptions = true;

        # Download Settings
        downloadBehavior = "download";

        # SponsorBlock Settings
        useSponsorBlock = true;
        useDeArrowTitles = true;
        useDeArrowThumbnails = true;
      };

    };
  };
}
