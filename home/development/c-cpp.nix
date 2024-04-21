{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [
      bear # For creating compilation databases for clangd
      cmake # Needed for exercism C++ tests
      gnumake # Needed for exercism C and C++ tests
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
  programs.helix = {
    extraPackages = with pkgs; [
      clang-tools # Provides clangd lsp
    ];
    # Use same config for C and C++
    languages.language =
      map
        (name: {
          inherit name;
          auto-format = true;
          indent = {
            tab-width = 4;
            unit = "    ";
          };
        })
        [
          "c"
          "cpp"
        ];
  };
}
