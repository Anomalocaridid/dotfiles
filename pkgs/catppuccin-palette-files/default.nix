{ stdenvNoCC
, fetchFromGitHub
}:
stdenvNoCC.mkDerivation {
  name = "catppuccin-palette-files";

  src = fetchFromGitHub {
    owner = "catppuccin";
    repo = "palette";
    rev = "205dd54c6158b7648621cf9fd00e91f03888ce7e";
    hash = "sha256-y14fd8lvnG9hNY6CRU0JgxWouexEw91aIEMkr1NaM/4=";
  };

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share
    cp -r $src/* $out/share
    
    runHook postInstall
  '';
}
