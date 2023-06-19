{ stdenvNoCC
, fetchzip
, lib
}:

stdenvNoCC.mkDerivation {
  pname = "cyberre-grub-theme";
  version = "1.0.0";

  src = fetchzip {
    url = "https://github.com/alealexpro100/various_files/raw/master/Grub2-theme%20CyberRe%201.0.0.tar.gz";
    hash = "sha256-RWEQHqWjSZtDlFjJlsQBig/bBaZL/srfez1qTzE+Qrw=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/grub/themes
    cp -r $src/CyberRe $out/grub/themes
    cp $src/CyberRe/theme.txt $out
    cp $src/CyberRe/*.png $out

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://www.opendesktop.org/s/Gnome/p/1420727";
    description = "CyberRe Grub theme";
    maintainers = with maintainers; [ anomalocaris ];
    platforms = platforms.linux;
  };
}
