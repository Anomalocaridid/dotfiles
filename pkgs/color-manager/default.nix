{
  lib,
  python3,
  python3Packages,
  fetchFromGitHub,
  custom, # NOTE: Remove if upstreaming
}:

python3.pkgs.buildPythonPackage {
  pname = "color-manager";
  version = "0-unstable-2024-09-04";

  src = fetchFromGitHub {
    owner = "NicklassVraa";
    repo = "Color-manager";
    rev = "d7d9d8f105bc68a6635e41961699e2045d760839";
    hash = "sha256-kXRjp1sFgSiIQC9+fUQcNRK990Hd5nZwJpRGB7qRYrY=";
  };

  propagatedBuildInputs = with python3Packages; [
    tqdm
    custom.basic-colormath
  ];

  meta = {
    description = "Recolor icon packs, themes, wallpapers and assets with a few clicks or lines of code";
    homepage = "https://github.com/NicklasVraa/Color-manager";
    license = lib.licenses.agpl3Only;
    maintainers = with lib.maintainers; [ anomalocaris ];
  };
}
