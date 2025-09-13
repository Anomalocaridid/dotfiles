{ config, ... }:
{
  unify.modules.general = {
    nixos =
      { pkgs, ... }:
      {
        programs.fish = {
          # Need to enable fish at system level to use as shell
          enable = true;
          # Translate bash scripts into fish rather than wrapping them with foreign-env, which is slower
          useBabelfish = true;
        };

        # Set fish as user shell
        users.users.${config.flake.meta.username}.shell = pkgs.fish;
      };

    home =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        programs.fish = {
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
                bind \cz --mode insert 'fg 2>/dev/null; commandline -f repaint'
              '';
          };
          interactiveShellInit = # fish
            ''
              # Use fish for `nix develop`
              ${lib.getExe pkgs.nix-your-shell} fish | source
            '';
          shellInit = # fish
            ''
              # Initialize batpipe
              eval (${lib.getExe pkgs.bat-extras.batpipe})

              # fzf-fish settings
              # width=20 so delta decorations don't wrap around small fzf preview pane
              # also disable side-by-side
              set --global fzf_diff_highlighter DELTA_FEATURES="+" delta --paging=never --width=20
            '';
          # Pipe every command run with --help through bat
          shellAbbrs."--help" = {
            position = "anywhere";
            expansion = "--help | bat --plain --language=help";
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
                "exercism-cli-fish-wrapper"
                "fish-bd"
                "fish-you-should-use"
                "fzf-fish"
                "grc"
                "plugin-sudope"
              ];
        };

        # Needed for plugins
        home.packages = with pkgs; [
          libnotify # Needed for done
          grc # Needed for grc
        ];
      };
  };
}
