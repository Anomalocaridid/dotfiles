{
  lib,
  rustPlatform,
  fetchFromGitLab,
  python3Packages,
  unstableGitUpdater,
  rustfmt,
}:
rustPlatform.buildRustPackage {
  pname = "windowtolayer";
  version = "0-unstable-2025-01-10";

  src = fetchFromGitLab {
    domain = "gitlab.freedesktop.org";
    owner = "mstoeckl";
    repo = "windowtolayer";
    rev = "109680f28823c6a8a53128947b0406bd2e368db6";
    hash = "sha256-yfsJtncGDYmMq7l+5EBYQ6Jf22TkbRU0VEyOZtc9Lo8=";
  };

  cargoHash = "sha256-47PAROdvw4p1AhDsAlK/3yIZTxfKAzJ+Ey/xbtXsVq8=";

  nativeBuildInputs = with python3Packages; [
    python
    rustfmt
  ];

  passthru.updateScript = unstableGitUpdater {
    url = "https://gitlab.freedesktop.org/mstoeckl/windowtolayer.git";
  };

  meta = {
    description = "Transforms individual Wayland clients into wallpapers";
    homepage = "https://gitlab.freedesktop.org/mstoeckl/windowtolayer";
    mainProgram = "windowtolayer";
    license = lib.licenses.unfree; # No upstream license
    maintainers = with lib.maintainers; [ anomalocaris ];
  };
}
