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
      "Sync" # Syncthing
      "exercism" # Exercism
      "Projects" # Misc. programming
      "qmk_firmware" # QMK
      ".ssh" # SSH key
      # Caches and data to persist
      ".local/share/zoxide" # Zoxide history
      ".cache/tealdeer" # Tldr pages, prevents tealdeer redownloading them every time
      "quicklisp" # Quicklisp and related package installation
      ".cache/common-lisp" # Prevents recompilation for quicklisp, etc. on first launch of sbcl for every boot
      ".julia" # Prevents recompilation for julia repl and lsp
      ".unison" # Unison codebase, needs to be persistent as all added code ends up there
    ];
  };

}
