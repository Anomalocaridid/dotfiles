{ pkgs, ... }: {
  programs.zsh = {
    enable = true;

    initExtra = #shell
      ''    
    # Ascii Terminal greeting. 
    # Shows Linux distro and version in rainbow ascii art.
      echo -en "\e[1m"
      lsb_release --description --short | 
        tr --delete '"' |
        toilet --termwidth --font smslant --filter border --directory ${pkgs.figlet}/share/figlet |
        lolcat
      echo -e "\e[1m Welcome back, $USER!\e[0m" | lolcat
      # init batpipe
      eval "$(batpipe)"
      '';

    shellAliases = {
      rm = "rm --interactive";
      # advcpmv
      cp = "cp --interactive --progress-bar";
      mv = "cp --interactive --progress-bar";
      # bat
      bg = "batgrep";
      cat = "bat --paging=never";
      less = "bat --paging=always";
      man = "batman";
      diff = "batdiff";
      # skim
      search = ''
        sk --ansi \
           --delimiter ':' \
           --nth=3 \
           --cmd 'rg --color=always --line-number \"{}\"' \
           --preview 'bat --style=numbers --color=always --highlight-line {2} {1}' | \
           cut --delimiter=':' --fields=1 -
      '';
    };
    shellGlobalAliases = {
      # page through help text
      "-- --help" = "--help | bat --plain --language=help";
    };
    sessionVariables = {
      # bat
      PAGER = "bat";
      MANPAGER = "sh -c 'col --no-backspaces --spaces | bat --plain --language=man'";
      # git
      GIT_PAGER = "PAGER='bat --plain' delta";
    };
  };

  home.packages = with pkgs; [
    # Needed for greeting
    toilet
    lolcat
    lsb-release
    figlet # provides fonts for toilet
    # Needed for custom command
    ripgrep
  ];
}
