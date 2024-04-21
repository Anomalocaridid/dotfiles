{
  writeShellApplication,
  grimblast,
  swappy,
  fuzzel,
  ...
}:
writeShellApplication {
  name = "screenshot.sh";
  runtimeInputs = [
    grimblast
    swappy
    fuzzel
  ];
  text = ''
    entries=("Active" "Screen" "Output" "Area")
    selected=$(printf '%s\n' "''${entries[@]}" | fuzzel --dmenu)
    grimblast --wait 1 --notify save "''${selected,,}" - | swappy -f -
  '';
}
