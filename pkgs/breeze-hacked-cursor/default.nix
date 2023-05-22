{ stdenvNoCC
, fetchzip
, lib
}:

stdenvNoCC.mkDerivation {
  pname = "breeze-hacked-cursor";
  version = "unstable-2018-3-20";

  src = fetchzip {
    url = "https://code.jpope.org/jpope/breeze_cursor_sources/raw/master/breeze-hacked-cursor-theme.zip";
    hash = "sha256-K8Ive+VGqtViF4LrZuPwVl2QBmP38bGjetap202yUm8=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/icons/Breeze_Hacked
    cp -rf $src/* $out/share/icons/Breeze_Hacked

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://kver.wordpress.com/2015/01/09/curses-i-mean-cursors/";
    description = "Breeze Hacked cursor theme";
    maintainers = with maintainers; [ anomalocaris ];
    platforms = platforms.linux;
  };
}
