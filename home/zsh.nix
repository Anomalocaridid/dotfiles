{ pkgs, inputs, ... }: {
  programs = {
    zsh = {
      enable = true;
      enableAutosuggestions = true;

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

      # Fix ZVM's conflict with autopair
      zvm_after_init_commands=(autopair-init)

      # Hook for transient prompt in starship
      zle-line-init() {
        emulate -L zsh

        [[ $CONTEXT == start ]] || return 0

        while true; do
          zle .recursive-edit
          local -i ret=$?
          [[ $ret == 0 && $KEYS == $'\4' ]] || break
          [[ -o ignore_eof ]] || exit 0
        done

        local saved_prompt=$PROMPT
        local saved_rprompt=$RPROMPT
        # Use if transient prompt needs to be significantly different from main prompt
        #PROMPT='$(STARSHIP_CONFIG=~/.config/starship/config-transient.toml starship prompt ... )'
        PROMPT='$(starship module character --terminal-width="$COLUMNS" \
                                            --keymap="''${KEYMAP:-}" \
                                            --status="$STARSHIP_CMD_STATUS" \
                                            --pipestatus="''${STARSHIP_PIPE_STATUS[*]}" \
                                            --cmd-duration="''${STARSHIP_DURATION:-}" \
                                            --jobs="$STARSHIP_JOBS_COUNT")'
        RPROMPT=""
        zle .reset-prompt
        PROMPT=$saved_prompt
        RPROMPT=$saved_rprompt

        if (( ret )); then
          zle .send-break
        else
          zle .accept-line
        fi
        return ret
      }

      zle -N zle-line-init

      # Autoload zsh modules not enabled by default
      autoload zcalc           # Calculator program
      autoload zmv             # Move files that match a pattern  
      autoload -U tetriscurses # Tetris
      '';

      shellAliases = {
        rm = "rm --interactive";
        du = "dust";
        df = "duf";
        # advcpmv
        cp = "cp --interactive --progress-bar";
        mv = "mv --interactive --progress-bar";
        # bat
        bgrep = "batgrep";
        cat = "bat --paging=never";
        less = "bat --paging=always";
        man = "batman";
        diff = "batdiff";
        # zoxide
        cd = "z";
        # wezterm
        imgcat = "wezterm imgcat";
        # ssh
        ssh-setup = "eval $(ssh-agent -s) && ssh-add";
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
        # oh-my-zsh alias-finder
        ZSH_ALIAS_FINDER_AUTOMATIC = "true";
        # Fix ZVM's conflict with autopair
        AUTOPAIR_INHIBIT_INIT = 1;
        # Fix ZVM's conflict with starship transient prompt code
        ZVM_INIT_MODE = "sourcing";
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
    zoxide.enable = true;
  };

  home.packages = with pkgs; [
    # Needed for greeting
    toilet
    lolcat
    lsb-release
    figlet # provides fonts for toilet
    # Needed for custom command
    ripgrep
    du-dust
    duf
    fd
  ];
}
