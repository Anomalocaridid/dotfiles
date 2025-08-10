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
  flake.modules.homeManager.development =
    { pkgs, ... }:
    {
      home.file.".clang-format".source =
        (inputs.nixago.lib.${pkgs.system}.make {
          data = {
            BasedOnStyle = "LLVM";
            IndentWidth = 4;
            IndentCaseLabels = true;
            AlignConsecutiveDeclarations = true;
          };
          output = "clang-format";
          format = "yaml";
        }).configFile;
    };
}
