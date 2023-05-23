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
      "exercism" # Exercism
      "Projects" # Misc. programming
      "qmk_firmware" # QMK
      ".ssh" # SSH key
      "Sync" # Syncthing
      ".local/share/nyxt" # Nyxt, history, bookmarks, etc.
      # Non-critical caches and data to persist
      ".cache/common-lisp" # Prevents recompilation for quicklisp, etc. on first launch of sbcl for every boot
      ".cache/nyxt" # Nyxt browser cache
      ".cache/tealdeer" # Tldr pages, prevents tealdeer redownloading them every time
      ".julia" # Prevents recompilation for julia repl and lsp
      ".local/share/zathura" # Zathura bookmarks, etc.
      ".local/share/zoxide" # Zoxide history
      "quicklisp" # Quicklisp and related package installation
      ".unison" # Unison codebase, needs to be persistent as all added code ends up there
    ];
  };

}
