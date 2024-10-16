args@{ ... }:
# Import all nix files in directory into attrset with attrs corresponding to file names 
# Should ignore this file and assume no non-nix files
builtins.foldl' (
  acc: x: acc // { ${builtins.head (builtins.split "\.nix" x)} = import ./${x} args; }
) { } (builtins.filter (file: file != "default.nix") (builtins.attrNames (builtins.readDir ./.)))
