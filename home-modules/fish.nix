{
  lib,
  pkgs,
  ...
}:
{
  programs = {
    fish = {
      enable = true;
      functions = {
        fish_greeting =
          let
            lolcat = lib.getExe pkgs.lolcat;
          in
          # fish
          ''
            # Ascii Terminal greeting. 
            # Shows Linux distro and version in rainbow ascii art.
            echo -en "\e[1m"
            ${lib.getExe pkgs.lsb-release} --description --short | 
              tr --delete '"' |
              ${lib.getExe pkgs.toilet} \
                --termwidth \
                --font smslant \
                --filter border \
                --directory ${pkgs.figlet}/share/figlet |
                ${lolcat}
            echo -e "\e[1m Welcome back, $USER!\e[0m" |
              ${lolcat}
          '';
        fish_user_key_bindings = # fish
          ''
            # Vi keybindings
            fish_vi_key_bindings
              
            # Make Ctrl+Z also bring program to foreground
            bind \cz -M insert 'fg 2>/dev/null; commandline -f repaint'
          '';
      };
      shellInit = # fish
        ''
          # Initialize batpipe
          eval (batpipe)

          # fzf-fish settings
          # width=20 so delta decorations don't wrap around small fzf preview pane
          # also disable side-by-side
          set -g fzf_diff_highlighter DELTA_FEATURES="+" delta --paging=never --width=20
        '';
      shellAbbrs = {
        # Pipe every command run with --help through bat
        "--help" = {
          position = "anywhere";
          expansion = "--help | bat --plain --language=help";
        };
      };
      shellAliases = {
        rd = "rmdir";
        md = "mkdir";
        rm = "rm --interactive";
        du = "${lib.getExe pkgs.du-dust}";
        df = "${lib.getExe pkgs.duf}";
        # bat
        bgrep = "batgrep";
        cat = "bat --paging=never";
        less = "bat --paging=always";
        man = "batman";
        diff = "batdiff";
        # wezterm
        imgcat = "wezterm imgcat";
        # tealdeer
        tldr = "PAGER='bat --plain' command tldr";
        # xplr
        xcd = "cd (xplr --print-pwd-as-result)";
        # lazygit
        lg = "lazygit";
      };
      plugins =
        map
          (name: {
            inherit name;
            src = pkgs.fishPlugins."${name}".src;
          })
          [
            "autopair"
            "done"
            "fish-bd"
            "fish-you-should-use"
            "fzf-fish"
            "grc"
            "plugin-sudope"
          ];
    };
    ripgrep.enable = true;
    fd.enable = true;
  };

  # Needed for plugins
  home.packages = with pkgs; [
    libnotify # Needed for done
    grc # Needed for grc
  ];
}
