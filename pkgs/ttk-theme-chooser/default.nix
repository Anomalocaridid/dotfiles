{
  fetchFromGitHub,
  tcl,
  tclPackages,
}:
tcl.mkTclDerivation rec {
  pname = "ttk-theme-chooser";
  version = "1.4";

  src = fetchFromGitHub {
    owner = "Geballin";
    repo = "TTK-theme-chooser";
    tag = version;
    hash = "sha256-eI+/oVLJI3/RAWoSF6rUDYnL6wwEQySZf0iiz9IsdtI=";
  };

  nativeBuildInputs = [
    tclPackages.tcllib
    tclPackages.tcl
    tclPackages.tk
  ];
}
