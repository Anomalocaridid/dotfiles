{ config, ... }: {
  home.persistence."/persist/${config.home.homeDirectory}" = {
    allowOther = true;
    directories = [
      # Default directories I care about
      "Documents"
      "Downloads"
      "Music"
      "Pictures"
      "Videos"
      # Gaming-specific stuff
      ".cache/lutris" # Lutris banner cache
      ".config/lutris" # Lutris games and settings
      ".local/share/lutris" # Lutris runtime data
      ".local/share/PrismLauncher" # Prism Launcher data
      ".local/share/Steam" # Steam games and save data
      ".PySolFC" # PySolFC settings and save data
      # Other important stuff
      ".config/FreeTube" # Freetube user data
      ".config/weechat" # .config/weechat/sec.conf has secrets that shouldn't be managed like a normal dotfile 
      "exercism" # Exercism
      ".local/share/nyxt" # Nyxt, history, bookmarks, etc.
      ".mozilla" # Firefox data
      "Projects" # Misc. programming
      "qmk_firmware" # QMK
      ".ssh" # SSH key
      "Sync" # Syncthing
      "Zotero" # Zotero databases
      # Non-critical caches and data to persist
      ".cache/common-lisp" # Prevents recompilation for quicklisp, etc. on first launch of sbcl for every boot
      ".cache/keepassxc" # KeePassXC cache
      ".cache/nyxt" # Nyxt browser cache
      ".cache/tealdeer" # Tldr pages, prevents tealdeer redownloading them every time
      ".config/exercism" # Exercism API key
      ".config/GIMP" # GIMP settings
      ".config/keepassxc" # KeePassXC settings
      ".config/LanguageTool" # LanguageTool settings
      ".config/libreoffice" # Libreoffice settings
      ".config/spotify" # Spotify user data
      ".cache/spotify" # Spotify cache
      ".config/strawberry" # Strawberry settings
      ".config/syncthing" # Syncthing settings
      ".config/WebCord" # Webcord user data
      ".julia" # Prevents recompilation for julia repl and lsp
      ".local/share/com.yubico.authenticator" # Yubico auth settings (may have secrets?)
      ".local/share/strawberry" # Strawberry cache
      ".local/share/weechat" # Weechat logs (plugins too?)
      ".local/share/zathura" # Zathura bookmarks, etc.
      ".local/share/zoxide" # Zoxide history
      "quicklisp" # Quicklisp and related package installation
      ".unison" # Unison codebase, needs to be persistent as all added code ends up there
    ];
  };

}
