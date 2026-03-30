{ inputs, ... }:
{
  # Catppuccin userstyles for Stylus
  flake-file.inputs.catppuccin-userstyles-nix = {
    url = "github:different-name/catppuccin-userstyles-nix?rev=b347a087e34ddb4ce645014744b101f217350209";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  unify.modules.general.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      # Force Firefox Color settings
      catppuccin.librewolf.force = true;

      programs.librewolf = {
        policies.ExtensionSettings =
          let
            mkExtensions =
              ids:
              lib.genAttrs ids (id: {
                install_url = "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";
                installation_mode = "force_installed";
                private_browsing = true;
              });
          in
          mkExtensions [
            # Firefox color
            "FirefoxColor@mozilla.com"
            # Catppuccin for Web File Explorer Icons
            "{bbb880ce-43c9-47ae-b746-c3e0096c5b76}"
            # Dark Reader
            "addon@darkreader.org"
            # Stylus
            "{7a7a4a92-a2a0-41d1-9fd7-1e92480d612d}"
          ];

        profiles.default.settings =
          let
            accent = config.catppuccin.accent;
            palette = config.catppuccin.sources.parsedPalette;
          in
          {
            "addon@darkreader.org" = {
              force = true;
              settings = {
                syncSettings = false;
                theme = {
                  fontFamily = builtins.head config.fonts.fontconfig.defaultFonts.monospace;
                  darkSchemeBackgroundColor = palette.base.hex;
                  darkSchemeTextColor = palette.text.hex;
                  selectionColor = palette.surface2.hex;
                };
                previewNewDesign = true;
              };
            };

            # Stylus
            "{7a7a4a92-a2a0-41d1-9fd7-1e92480d612d}" = {
              force = true;
              settings = inputs.catppuccin-userstyles-nix.stylusSettings.${pkgs.stdenv.hostPlatform.system} {
                global = {
                  lightFlavor = config.catppuccin.flavor;
                  darkFlavor = config.catppuccin.flavor;
                  accentColor = config.catppuccin.accent;
                };
              };
            };

            # Configure UBlacklist highlight colors
            "@ublacklist".settings = {
              linkColor = palette.blue.hex;
              blockColor = palette.red.hex;
              highlightColors = [ palette.${accent}.hex ];
            };
          };
      };
    };
}
