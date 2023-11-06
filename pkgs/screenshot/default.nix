{ writeShellApplication
, grimblast
, swappy
, ...
}:
writeShellApplication {
  name = "screenshot.sh";
  runtimeInputs = [
    grimblast
    swappy
  ];
  text = ''        
    entries=("Active" "Screen" "Output" "Area")
    selected=$(printf '%s\n' "''${entries[@]}" | wofi --show dmenu --lines 7)
    grimblast --notify save "''${selected,,}" - | swappy -f -
  '';
}
