{
  perSystem =
    { pkgs, ... }:
    {
      devshells.qml = {
        packages = with pkgs; [
          # Provides qmlls, the QML language server and qmlformat, the formatter
          kdePackages.qtdeclarative
        ];
      };
    };

  # Needed to detect libraries from environment variables
  unify.modules.general.home.programs.helix.languages = {
    language = [
      {
        name = "qml";
        # For some reason, it does not play well with auto-format
        formatter = {
          command = "qmlformat";
          args = [
            "--normalize"
            "--objects-spacing"
            "--functions-spacing"
            "--sort-imports"
            "%{buffer_name}"
          ];
        };
      }
    ];

    # Needed to help detect import paths from environment
    language-server.qmlls.args = [ "-E" ];
  };
}
