{ stdenvNoCC
, fetchzip
, lib
}:

stdenvNoCC.mkDerivation {
  pname = "gtk-materia-cyberpunk-neon";
  version = "unstable-2021-12-16";

  src = fetchzip {
    url = "https://github.com/Roboron3042/Cyberpunk-Neon/raw/master/gtk/materia-cyberpunk-neon.zip";
    hash = "sha256-d+BduU4ZNHQjDbEfmVaYeuBTW9Z37QvYa0UOZwGukco=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/themes/materia-cyberpunk-neon
    cp -rf $src/* $out/share/themes/materia-cyberpunk-neon

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/Roboron3042/Cyberpunk-Neon";
    description = "GTK Cyberpunk Theme credit to Roboron3042 @github.com";
    license = licenses.cc-by-sa-40;
    maintainers = with maintainers; [ anomalocaris ];
    platforms = platforms.linux;
  };
}
