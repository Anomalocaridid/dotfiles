{ stdenvNoCC
, fetchFromGitHub
, lib
}:

stdenvNoCC.mkDerivation {
  pname = "candy-icons";
  version = "unstable-2023-5-8";

  src = fetchFromGitHub {
    owner = "EliverLara";
    repo = "candy-icons";
    rev = "b6fde4c1de7a713aa561c039c1b26ef5169a6318";
    hash = "sha256-BoHkDdgvZ5bzrvij5FvG7xj53gfp+zIuWp6AnS8rurk=";
  };

  # TODO: Separate into two packages, have candy-icons as dependency for sweet-folders?

  folders = fetchFromGitHub {
    owner = "EliverLara";
    repo = "Sweet-folders";
    rev = "b2192ff1412472f036fdf9778c6b9dbcb6c044ec";
    hash = "sha256-QexfqXH5a1IEhKBRjWSMdrEvThvLRzd4M32Xti1DCGE=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/icons/candy-icons
    cp -rf $src/* $out/share/icons/candy-icons

    cp -rf $folders/Sweet-Rainbow $out/share/icons

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/EliverLara/candy-icons";
    description = "Sweet gradient icons";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ anomalocaris ];
    platforms = platforms.linux;
  };
}
