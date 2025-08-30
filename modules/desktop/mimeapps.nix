{ inputs, ... }:
{
  unify.modules.mimeapps.home =
    { pkgs, ... }:
    {
      xdg = {
        mimeApps.enable = true;

        configFile."handlr/handlr.toml".source =
          (inputs.nixago.lib.${pkgs.system}.make {
            data = {
              enable_selector = false;
              selector = "fuzzel --dmenu --prompt='Open With: '";
              handlers = [
                {
                  exec = "freetube %u";
                  regexes = [ "youtu(be.com|.be)" ];
                }
                {
                  exec = "handlr open steam://openurl/%u";
                  regexes = [ "^https://([[:alpha:]]*\.)?steam(powered|community).com/" ];
                }
              ];
            };
            output = "handlr.toml";
          }).configFile;
      };

      home.packages = with pkgs; [
        handlr-regex
        # Use handlr as drop-in replacement for xdg-open
        (writeShellApplication {
          name = "xdg-open";
          runtimeInputs = [ handlr-regex ];
          text = # shell
            ''
              handlr open -- "$@"
            '';
        })
        # Use handlr as drop-in replacement for xterm
        (writeShellApplication {
          name = "xterm";
          runtimeInputs = [ handlr-regex ];
          text = # shell
            ''
              handlr launch x-scheme-handler/terminal -- "$@"
            '';
        })
      ];
    };
}
