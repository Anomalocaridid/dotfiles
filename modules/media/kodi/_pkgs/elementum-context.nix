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
  pname = "elementum-context";
  namespace = "context.elementum";
  version = "0.0.15";

  src = fetchFromGitHub {
    owner = "elgatito";
    repo = namespace;
    tag = "v${version}";
    hash = "sha256-Wtx7CDcCVdb8d/9n4F54tbi52nvY/FWZKJsJW9+9xS4=";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out${addonDir}/${namespace}/
    cp -r ./ $out${addonDir}/${namespace}/
  '';

  nativeBuildInputs = [ python ];

  propagatedBuildInputs = [ future ];

  meta = {
    homepage = "https://elementum.surge.sh/context";
    description = "Context menu options for Elementum";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ anomalocaris ];
    teams = lib.teams.kodi;
  };
}
