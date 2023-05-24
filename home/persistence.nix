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
      # Other important stuff
      ".config/weechat" # .config/weechat/sec.conf has secrets that shouldn't be managed like a normal dotfile 
      "exercism" # Exercism
      ".local/share/nyxt" # Nyxt, history, bookmarks, etc.
      "Projects" # Misc. programming
      "qmk_firmware" # QMK
      ".ssh" # SSH key
      "Sync" # Syncthing
      # Non-critical caches and data to persist
      ".cache/common-lisp" # Prevents recompilation for quicklisp, etc. on first launch of sbcl for every boot
      ".cache/keepassxc" # KeePassXC cache
      ".cache/nyxt" # Nyxt browser cache
      ".cache/tealdeer" # Tldr pages, prevents tealdeer redownloading them every time
      ".julia" # Prevents recompilation for julia repl and lsp
      ".local/share/weechat" # Weechat logs (plugins too?)
      ".local/share/zathura" # Zathura bookmarks, etc.
      ".local/share/zoxide" # Zoxide history
      "quicklisp" # Quicklisp and related package installation
      ".unison" # Unison codebase, needs to be persistent as all added code ends up there
      ".local/share/com.yubico.authenticator" # Yubico auth settings (may have secrets?)
    ];
  };

}
