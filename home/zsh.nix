{ pkgs, inputs, ... }: {
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

      # Init batpipe
      eval "$(batpipe)"

      # Completion settings
      zstyle ":completion:*" rehash true                                            # Persistent rehash
      zstyle ":completion:*" matcher-list "m:{a-zA-z}={A-Za-z}"                     # Case-insensitive completion

      # fzf-tab zstyles
      zstyle ":completion:complete:*:options" sort false                            # Disable sort when completing options
      zstyle ":completion:*:git-checkout:*" sort false                              # Disable sort when completing git branches
      zstyle ":completion:*:descriptions" format "[%d]"                             # Add descriptions when able
      zstyle ":completion:*" list-colors "''${(s.:.)LS_COLORS}"                       # color output with LS_COLORS
      zstyle ":fzf-tab:complete:cd:*" fzf-preview 'exa -1 --color=always $realpath' # Preview directories with exa
      zstyle ":fzf-tab:*" fzf-flags "$SKIM_DEFAULT_OPTIONS"                         # Since skim is being instead of fzf, use skim"s default flags

      # Fix ZVM's conflict with autopait
      zvm_after_init_commands=(autopair-init)
      '';

    shellAliases = {
      rm = "rm --interactive";
      # advcpmv
      cp = "cp --interactive --progress-bar";
      mv = "cp --interactive --progress-bar";
      # bat
      bgrep = "batgrep";
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
      # oh-my-zsh alias-finder
      ZSH_ALIAS_FINDER_AUTOMATIC = "true";
      # Fix ZVM's conflict with autopait
      AUTOPAIR_INHIBIT_INIT = 1;
    };

    plugins = with pkgs; [
      {
        name = "fzf-tab";
        src = "${zsh-fzf-tab}/share/fzf-tab";
      }
      {
        name = "autopair";
        file = "autopair.zsh";
        src = "${zsh-autopair}/share/zsh/zsh-autopair";
      }
      {
        name = "bd";
        src = inputs.zsh-bd;
      }
      {
        name = "fast-syntax-highlighting";
        src = "${zsh-fast-syntax-highlighting}/share/zsh/site-functions";
      }
      {
        name = "zsh-vi-mode";
        src = "${zsh-vi-mode}/share/zsh-vi-mode";
      }
    ];

    defaultKeymap = "viins";

    historySubstringSearch = {
      enable = true;
    };

    oh-my-zsh = {
      enable = true;
      plugins = [
        "alias-finder"
        "fancy-ctrl-z"
        "sudo"
      ];
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
