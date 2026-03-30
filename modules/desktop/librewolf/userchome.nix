{ inputs, ... }:
{
  # Firefox userChrome
  flake-file.inputs.cascade = {
    url = "github:cascadefox/cascade";
    flake = false;
  };

  unify.modules.general.home =
    { config, lib, ... }:
    {
      programs.librewolf = {
        policies.Preferences =
          let
            mkPreferences =
              prefs:
              lib.mergeAttrsList
              <| lib.mapAttrsToListRecursive (path: value: {
                "${lib.concatStringsSep "." path}" = {
                  Value = value;
                  Status = "locked";
                };
              }) prefs;
          in
          mkPreferences {
            # Enable userChrome
            toolkit.legacyUserProfileCustomizations.stylesheets = true;

            # Disable Alt key menu (somehow breaks cascade userChrome with persistent effects)
            ui.key.menuAccessKeyFocuses = false;
          };

        profiles.default.userChrome = # css
          ''
            /* Default settings */
            @import "${inputs.cascade}/chrome/includes/cascade-config.css";

            @import "${inputs.cascade}/chrome/includes/cascade-layout.css";
            @import "${inputs.cascade}/chrome/includes/cascade-responsive.css";
            @import "${inputs.cascade}/chrome/includes/cascade-floating-panel.css";

            @import "${inputs.cascade}/chrome/includes/cascade-nav-bar.css";
            @import "${inputs.cascade}/chrome/includes/cascade-tabs.css";

            /* Not default settings */
            @import "${inputs.cascade}/integrations/catppuccin/cascade-${config.catppuccin.flavor}.css";
          '';
      };
    };
}
