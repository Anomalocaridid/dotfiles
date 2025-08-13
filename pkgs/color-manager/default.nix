{
  lib,
  python3,
  python3Packages,
  fetchFromGitHub,
  custom, # NOTE: Remove if upstreaming
}:

python3.pkgs.buildPythonPackage {
  pname = "color-manager";
  version = "0-unstable-2024-11-14";

  src = fetchFromGitHub {
    owner = "NicklasVraa";
    repo = "Color-manager";
    rev = "9e55e0971ecd0e3141ed5d7d9a8377f7052cef96";
    hash = "sha256-kXRjp1sFgSiIQC9+fUQcNRK990Hd5nZwJpRGB7qRYrY=";
  };

  propagatedBuildInputs = with python3Packages; [
    tqdm
    custom.basic-colormath
  ];

  pyproject = true;
  build-system = with python3Packages; [ setuptools ];

  meta = {
    description = "Recolor icon packs, themes, wallpapers and assets with a few clicks or lines of code";
    homepage = "https://github.com/NicklasVraa/Color-manager";
    license = lib.licenses.agpl3Only;
    maintainers = with lib.maintainers; [ anomalocaris ];
  };
}
