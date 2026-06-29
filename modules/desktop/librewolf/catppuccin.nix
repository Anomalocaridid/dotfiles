{ inputs, ... }:
{
  # Catppuccin userstyles for Stylus
  flake-file.inputs.catppuccin-userstyles = {
    url = "github:catppuccin/userstyles";
    flake = false;
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
        policies = {
          ExtensionSettings =
            let
              mkExtension = install_url: {
                inherit install_url;
                installation_mode = "force_installed";
                private_browsing = true;
              };

              mkAMOExtension =
                id: mkExtension "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";

              mkFEDExtension =
                packageName:
                let
                  package =
                    inputs.firefox-extensions-declarative.packages.${pkgs.stdenv.hostPlatform.system}.${packageName};
                  id = package.extensionId;
                in
                lib.nameValuePair id (
                  mkExtension "file://${package}/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/${id}.xpi"
                );
            in
            (lib.genAttrs [
              # Firefox color
              "FirefoxColor@mozilla.com"
              # Catppuccin for Web File Explorer Icons
              "{bbb880ce-43c9-47ae-b746-c3e0096c5b76}"
            ] mkAMOExtension)
            // (lib.genAttrs' [
              # Dark Reader
              "darkreader-declarative"
              # Stylus
              "stylus-declarative"
            ] mkFEDExtension);

          "3rdparty".Extensions =
            let
              palette = config.catppuccin.sources.parsedPalette;
              userstyleDir = "${inputs.catppuccin-userstyles}/styles";
            in
            {
              # Dark Reader
              "addon@darkreader.org" = {
                # Enable Dark Reader on protected pages like the Firefox web store
                # Unfortunately does not work on `about:` pages
                enableForProtectedPages = true;
                # Remove annoying news banner in Dark Reader menu
                fetchNews = false;
                # Required for theme settings
                previewNewDesign = true;
                # Do not sync settings
                syncSettings = false;
                theme = {
                  useFont = true;
                  fontFamily = builtins.head config.fonts.fontconfig.defaultFonts.sansSerif;
                  darkSchemeBackgroundColor = palette.base.hex;
                  darkSchemeTextColor = palette.text.hex;
                  selectionColor = palette.surface2.hex;
                  styleSystemControls = true;
                };
              };

              # Stylus
              "{7a7a4a92-a2a0-41d1-9fd7-1e92480d612d}" = {
                styles = lib.mapAttrsToList (name: _: {
                  code = builtins.readFile "${userstyleDir}/${name}/catppuccin.user.less";
                  variables = {
                    lightFlavor = config.catppuccin.flavor;
                    darkFlavor = config.catppuccin.flavor;
                    accentColor = config.catppuccin.accent;
                  };
                }) (builtins.readDir userstyleDir);
              };
            };
        };

        profiles.default.extensions.settings =
          let
            accent = config.catppuccin.accent;
            palette = config.catppuccin.sources.parsedPalette;
          in
          {
            # Configure UBlacklist highlight colors
            "@ublacklist".settings = {
              linkColor = palette.blue.hex;
              blockColor = palette.red.hex;
              highlightColors = [ palette.${accent}.hex ];
            };

            # Configure uBlock Origin accent color
            # NOTE: policies are used for all other configured settings
            "uBlock0@raymondhill.net".settings = {
              force = true;
              uiAccentCustom = true;
              uiAccentCustom0 = palette.${accent}.hex;
              # NOTE: Librewolf intentionally does not tell sites to prefer dark theme to prevent fingerprinting
              uiTheme = "dark";
            };
          };
      };
    };
}
