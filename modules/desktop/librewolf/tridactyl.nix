{
  unify.modules.general.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      programs.librewolf = {
        nativeMessagingHosts = with pkgs; [ tridactyl-native ];
        policies.ExtensionSettings =
          let
            id = "tridactyl.vim@cmcaine.co.uk";
          in
          {
            ${id} = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";
              installation_mode = "force_installed";
              private_browsing = true;
            };
          };
      };

      # Tridactyl uses a separate config file
      xdg.configFile."tridactyl/tridactylrc".text =
        let
          theme = "catppuccin-${config.catppuccin.flavor}";
        in
        ''
          " Clear commandline history and config not present in config file
          sanitize commandline tridactylsync tridactyllocal

          " Set colorscheme
          " TODO: change URL when repo is made official: https://github.com/catppuccin/catppuccin/issues/2799
          colourscheme --url https://raw.githubusercontent.com/devnullvoid/tridactyl/catppuccin-review-changes/themes/${theme}.css ${theme}

          " Set new tab page
          set newtab ${config.programs.librewolf.policies.Preferences."browser.startup.homepage".Value}

          " Set text editor for editing text fields
          set editorcmd handlr launch text/plain --

          " Adjust keybinds to account for vertical tabs
          bind J tabnext
          bind K tabprev
        '';
    };
}
