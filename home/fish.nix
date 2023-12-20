{ lib, pkgs, inputs, ... }: {
  programs = {
    fish = {
      enable = true;
      catppuccin.enable = true;
      functions = {
        fish_greeting = # fish
          ''
            # Ascii Terminal greeting. 
            # Shows Linux distro and version in rainbow ascii art.
            echo -en "\e[1m"
            ${pkgs.lsb-release}/bin/lsb_release --description --short | 
              tr --delete '"' |
              ${pkgs.toilet}/bin/toilet \
                --termwidth \
                --font smslant \
                --filter border \
                --directory ${pkgs.figlet}/share/figlet |
                ${pkgs.lolcat}/bin/lolcat
            echo -e "\e[1m Welcome back, $USER!\e[0m" |
              ${pkgs.lolcat}/bin/lolcat
          '';
        fish_user_key_bindings = # fish
          ''
            # Vi keybindings
            fish_vi_key_bindings
            
            # Make Ctrl+Z also bring program to foreground
            bind \cz -M insert 'fg 2>/dev/null; commandline -f repaint'
          '';
      };
      interactiveShellInit = # fish
        ''
          # Pipe every command run with --help through bat
          abbr --add --position anywhere -- --help "--help | bat --plain --language=help" 

          # Initialize batpipe
          eval (batpipe)

          # fzf-fish settings
          # width=20 so delta decorations don't wrap around small fzf preview pane
          # also disable side-by-side
          set -g fzf_diff_highlighter DELTA_FEATURES="+" delta --paging=never --width=20
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
        # tealdeer
        tldr = "PAGER='bat --plain' tldr";
      };
      plugins = with pkgs;
        let
          pluginFromPkgs = name: {
            inherit name;
            src = fishPlugins."${name}".src;
          };
          pluginFromInputs = name: {
            inherit name;
            src = inputs."${name}";
          };
        in
        lib.concatLists [
          (map pluginFromPkgs [
            "autopair"
            "done"
            "fzf-fish"
            "grc"
          ])
          (map pluginFromInputs [
            "fish-bd"
            "plugin-sudope"
            "you-should-use"
          ])
        ];
    };
    zoxide.enable = true;
  };

  home.packages = with pkgs; [
    # Needed for plugins
    libnotify # Needed for done
    grc # Needed for grc
    # Needed for custom command
    du-dust
    duf
    fd
    ripgrep
    custom.advcpmv-coreutils # add progress bars to cp and mv (depends on overlay in flake.nix)
  ];
}

