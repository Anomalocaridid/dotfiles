{
  lib,
  fetchFromGitHub,
  cmake,
  pkg-config,
  tcl,
  tclPackages,
  gtk2,
  gtk_engines,
  gdk-pixbuf-xlib,
}:
tcl.mkTclDerivation rec {
  pname = "gtkTtk";
  version = "0.9";

  src = fetchFromGitHub {
    owner = "Geballin";
    repo = "gtkTtk";
    tag = version;
    hash = "sha256-gSqd9FatF4q06r5TrhSGVJ+vh0ybNv8/9AP68v7pW1E=";
  };

  # For some reason, private Tk headers are required but Tk's src is a tarball, so it needs to be unpacked
  postUnpack = ''
    unpackFile "${tclPackages.tk.src}"
  '';

  # Do not load GTK dynamically because it relies on hard-coded paths
  postPatch = ''
    substituteInPlace CMakeLists.txt \
      --replace-fail 'SET ( LOAD_GTK_DYNAMICALLY ON )' 'SET ( LOAD_GTK_DYNAMICALLY OFF )'
  '';

  cmakeFlags = [
    (lib.cmakeFeature "CMAKE_POLICY_VERSION_MINIMUM" "3.5")
  ];

  # For some reasons, these directories are not being found automatically
  env.NIX_CFLAGS_COMPILE = toString [
    "-I${gdk-pixbuf-xlib.dev}/include/gdk-pixbuf-2.0"
    # Tk's private headers are required
    "-I/build/tk${tclPackages.tk.version}/generic/ttk"
  ];

  buildInputs = [
    cmake
    pkg-config
    tclPackages.tk
    gtk2
    gtk_engines
    gdk-pixbuf-xlib
  ];
}
