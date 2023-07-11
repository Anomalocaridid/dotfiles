{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      bear # For creating compilation databases for clangd
      clang-tools # Provides clangd lsp
      cmake # Needed for exercism C++ tests
    ];
    file.".clang-format".source =
      let
        yamlFormat = pkgs.formats.yaml { };
      in
      yamlFormat.generate "clang-format" {
        BasedOnStyle = "LLVM";
        IndentWidth = 4;
        IndentCaseLabels = true;
        AlignConsecutiveDeclarations = true;
      };
  };
}
