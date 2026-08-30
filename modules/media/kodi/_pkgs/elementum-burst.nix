{
  lib,
  buildKodiAddon,
  fetchFromGitHub,
  kodi,
  addonDir,
  future,
}:
let
  python = kodi.pythonPackages.python.withPackages (p: with p; [ flake8 ]);
in
buildKodiAddon rec {
  pname = "elementum-burst";
  namespace = "script.elementum.burst";
  version = "0.0.99";

  src = fetchFromGitHub {
    owner = "elgatito";
    repo = namespace;
    tag = "v${version}";
    hash = "sha256-EnE/NAMOMznsfC5wbD2keZ8aL6f8b+44iTGnU9bv+Sw=";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out${addonDir}/${namespace}/
    cp -r ./ $out${addonDir}/${namespace}/
  '';

  nativeBuildInputs = [ python ];

  propagatedBuildInputs = [ future ];

  meta = {
    homepage = "https://elementum.surge.sh/burst";
    description = "Torrent providers for Elementum";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ anomalocaris ];
    teams = lib.teams.kodi;
  };
}
