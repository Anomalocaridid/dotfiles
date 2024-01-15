{ lib
, stdenvNoCC
, fetchFromGitHub
, inkscape
, xcursorgen
, accentColor ? null # cyan details
, baseColor ? null # dark background
, borderColor ? null # grey border
, textColor ? null # Used in X and Wayland logo cursors
, ...
}:
stdenvNoCC.mkDerivation {
  pname = "breeze-hacked-cursor";
  version = "unstable-2023-07-11";

  src = fetchFromGitHub {
    owner = "clayrisser";
    repo = "breeze-hacked-cursor-theme";
    rev = "9108292b318939f0b0ba8adb761cc26e9c612326";
    hash = "sha256-6I41RpsB5dJBO4kMAKNtfmrRzbinrK4eyO22+bTOW9s=";
  };

  nativeBuildInputs = [ inkscape xcursorgen ];

  postPatch =
    let
      colors = [
        accentColor
        baseColor
        borderColor
        textColor
      ];
    in
    ''
      patchShebangs build.sh
      substituteInPlace Makefile \
        --replace "~/.icons" "$out/share/icons"
    '' + lib.optionalString (builtins.any (x: x != null) colors) ''
      substituteInPlace src/cursors.svg \
    '' + lib.optionalString (accentColor != null) ''
      --replace "#79f5f3" "${accentColor}" \
    '' + lib.optionalString (baseColor != null) ''
      --replace "#192629" "${baseColor}" \
    '' + lib.optionalString (borderColor != null) ''
      --replace "#666666" "${borderColor}" \
    '' + lib.optionalString (textColor != null) ''
      --replace "#fcfcfc" "${textColor}" \
      --replace "#ffffff" "${textColor}"
    '';

  meta = with lib;
    {
      homepage = "https://github.com/clayrisser/breeze-hacked-cursor-theme";
      description = "Breeze Hacked cursor theme";
      license = licenses.gpl2;
      maintainers = with maintainers; [ anomalocaris ];
      platforms = platforms.linux;
    };
}
