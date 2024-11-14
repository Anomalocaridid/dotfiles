{
  lib,
  python3,
  python3Packages,
  fetchPypi,
}:
python3.pkgs.buildPythonPackage rec {
  pname = "basic-colormath";
  version = "0.5.0";
  pyproject = true;

  src = fetchPypi {
    inherit version;
    pname = "basic_colormath";
    hash = "sha256-p/uNuNg5kqKIkeMmX5sWY8umGAg0E4/otgQxhzIuo0E=";
  };

  propagatedBuildInputs = with python3Packages; [
    setuptools
    setuptools-scm
    pillow
  ];

  meta = {
    description = "Simple color conversion and perceptual (DeltaE CIE 2000) difference";
    homepage = "https://github.com/ShayHill/basic_colormath";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ anomalocaris ];
  };
}
