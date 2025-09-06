{
  perSystem =
    { pkgs, ... }:
    {
      devshells.d.packages = with pkgs; [
        dmd # D compiler
        dub # D build tool
        serve-d # D language server
      ];
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "d";
      auto-format = true;
    }
  ];
}
