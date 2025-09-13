{ inputs, ... }:
{
  # Extra Catppuccin themes
  flake-file.inputs.catppuccin-ohmyrepl = {
    url = "github:catppuccin/ohmyrepl";
    flake = false;
  };

  perSystem =
    { pkgs, ... }:
    {
      devshells.julia.packages = with pkgs; [
        (julia.withPackages [
          "Crayons" # Needed for OhMyREPL color scheme
          "LanguageServer"
          "OhMyREPL"
        ])
      ];
    };

  unify.modules.general.home =
    { config, lib, ... }:
    {
      home.file.".julia/config/startup.jl".text =
        let
          mkUpper =
            str:
            (lib.toUpper (builtins.substring 0 1 str)) + (builtins.substring 1 (builtins.stringLength str) str);
        in
        # julia
        ''
          import OhMyREPL
          include("${inputs.catppuccin-ohmyrepl}/catppuccin.jl")
          OhMyREPL.colorscheme!("Catppuccin${mkUpper config.catppuccin.flavor}")
        '';

      programs.helix.languages.language = [
        {
          name = "julia";
          auto-format = true;
        }
      ];
    };
}
