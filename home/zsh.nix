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
    };
    sessionVariables = {
      # bat
      PAGER = "bat";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      # git
      GIT_PAGER = "PAGER=less delta";
    };
  };

  home.packages = with pkgs; [
    # Needed for greeting
    toilet
    lolcat
    lsb-release
    figlet # provides fonts for toilet
  ];
}
