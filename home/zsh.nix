{ pkgs, inputs, ... }: {
  programs = {
    zsh = {
      enable = true;
      enableAutosuggestions = true;

      initExtra = #sh
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

          # Hook for transient prompt in starship
          starship_zle-line-init() {
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

          hooks-add-hook zle_line_init_hook zvm_zle-line-init
          hooks-add-hook zle_line_init_hook starship_zle-line-init

          # Manually init or these won't work with zvm + zsh-hooks
          autopair-init # zsh-autopair
          bindkey '^Z' fancy-ctrl-z # Oh My Zsh's fancy-ctrl-z

          # Sync PWD with shell when exiting nnn's n alias
          source "${pkgs.nnn}/share/quitcd/quitcd.bash_zsh";

          # Sync subshell PWD with nnn
          nnn_cd() {
          	if [ -n "$NNN_PIPE" ]; then
          		printf "%s\0" "0c''${PWD}" ! >"''${NNN_PIPE}" &
          	fi
          }

          trap nnn_cd EXIT

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
        # glow
        notes = "glow $HOME/Sync/Notes";
        # skim
        search = ''
          sk --ansi \
             --delimiter ':' \
             --nth=3 \
             --cmd 'rg --color=always --line-number \"{}\"' \
             --preview 'bat --style=numbers --color=always --highlight-line {2} {1}' | \
             cut --delimiter=':' --fields=1 -
        '';
        # tealdeer
        tldr = "PAGER='bat --plain' tldr";
      };
      shellGlobalAliases = {
        # page through help text
        "-- --help" = "--help | bat --plain --language=help";
      };
      sessionVariables = {
        # Prevent initializing zsh-autopair twice since it's manually initialized in initExtra
        AUTOPAIR_INHIBIT_INIT = 1;
        # Fix ZVM's conflict with zsh-autopair
        ZVM_INIT_MODE = "sourcing";
        # Display alias notices after execution
        YSU_MESSAGE_POSITION = "after";
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
        {
          name = "zsh-hooks";
          src = inputs.zsh-hooks;
        }
        {
          name = "you-should-use";
          src = "${zsh-you-should-use}/share/zsh/plugins/you-should-use";
        }
      ];

      defaultKeymap = "viins";

      historySubstringSearch = {
        enable = true;
      };

      oh-my-zsh = {
        enable = true;
        plugins = [
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
    custom.advcpmv-coreutils # add progress bars to cp and mv (depends on overlay in flake.nix)
  ];
}
