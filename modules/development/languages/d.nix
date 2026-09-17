{
  perSystem = { pkgs, ... }: {
    devshells.d.packages = with pkgs; [
      dmd # D compiler
      dub # D build tool
      serve-d # D language server
    ];
  };

  flake.modules.homeManager.general.programs.helix.languages.language = [
    {
      name = "d";
      auto-format = true;
    }
  ];
}
