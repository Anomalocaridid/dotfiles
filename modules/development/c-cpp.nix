{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      devshells.c-cpp.packages = with pkgs; [
        bear # For creating compilation databases for clangd
        clang-tools # Provides clangd lsp
        cmake # Needed for exercism C++ tests
        gcc # Provides cc linker
        gnumake # Needed for exercism C and C++ tests
      ];
    };

  unify.modules.general.home =
    let
      indentWidth = 4;
    in
    { lib, pkgs, ... }:
    {
      home.file.".clang-format".source =
        (inputs.nixago.lib.${pkgs.system}.make {
          data = {
            BasedOnStyle = "LLVM";
            IndentWidth = indentWidth;
            IndentCaseLabels = true;
            AlignConsecutiveDeclarations = true;
          };
          output = "clang-format";
          format = "yaml";
        }).configFile;

      programs.helix.languages.language =
        let
          commonConfig = name: {
            inherit name;
            auto-format = true;
            indent = {
              tab-width = indentWidth;
              unit = lib.replicate indentWidth " " |> lib.concatStrings;
            };
          };
        in
        builtins.map commonConfig [
          "c"
          "cpp"
        ];
    };
}
