{
  unify.modules.general.home.programs.librewolf.policies =
    let
      id = "uBlock0@raymondhill.net";
    in
    {
      ExtensionSettings.${id} = {
        install_url = "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";
        installation_mode = "force_installed";
        private_browsing = true;
      };

      "3rdparty".Extensions.${id}.toOverwrite.filterLists = [
        # Built-in
        ## uBlock filters
        "ublock-filters"
        "ublock-badware"
        "ublock-privacy"
        "ublock-quick-fixes"
        "ublock-unbreak"
        ## uBlock filters - Experimental
        "ublock-experimental"

        # Ads
        "easylist"
        "adguard-generic"
        "adguard-mobile"

        # Privacy
        "easyprivacy"
        "LegitimateURLShortener"
        "adguard-spyware-url"
        "block-lan"

        # Malware protection, security
        "urlhaus-1"
        "curben-phishing"

        # Multipurpose
        "plowe-0"
        "dpollock-0"

        # Cookie notices
        ## EasyList/uBO - Cookie Notices
        "fanboy-cookiemonster"
        "ublock-cookies-easylist"
        ## AdGuard/uBO - Cookie Notices
        "adguard-cookies"
        "ublock-cookies-adguard"

        # Social widgets
        "fanboy-social"
        "adguard-social"
        "fanboy-thirdparty_social"

        # Annoyances
        ## EasyList - Annoyances
        "fanboy-ai-suggestions"
        "easylist-chat"
        "easylist-newsletters"
        "easylist-notifications"
        "easylist-annoyances"
        ## AdGuard - Annoyances
        "adguard-mobile-app-banners"
        "adguard-other-annoyances"
        "adguard-popup-overlays"
        "adguard-widgets"
        ## uBlock filters - Annoyances
        "ublock-annoyances"
      ];
    };
}
