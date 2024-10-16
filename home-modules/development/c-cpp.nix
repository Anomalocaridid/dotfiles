{ pkgs, ... }:
{
  home.file.".clang-format".source =
    let
      yamlFormat = pkgs.formats.yaml { };
    in
    yamlFormat.generate "clang-format" {
      BasedOnStyle = "LLVM";
      IndentWidth = 4;
      IndentCaseLabels = true;
      AlignConsecutiveDeclarations = true;
    };
}
