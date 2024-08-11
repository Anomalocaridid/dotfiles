{ lib, ... }:
{
  # Import all nix files in directory, ignoring self
  # Currently no non-nix files to worry about
  imports = map (file: ./. + "/${file}") (
    lib.strings.filter (file: file != "default.nix") (builtins.attrNames (builtins.readDir ./.))
  );
}
