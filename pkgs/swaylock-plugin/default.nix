# TODO: remove when NixOS/nixpkgs/pull/352330 makes it to unstable
{
  lib,
  stdenv,
  cairo,
  fetchFromGitHub,
  gdk-pixbuf,
  libxcrypt,
  libxkbcommon,
  meson,
  ninja,
  nix-update-script,
  pam,
  pkg-config,
  scdoc,
  versionCheckHook,
  wayland,
  wayland-protocols,
  wayland-scanner,
  systemd,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "swaylock-plugin";
  version = "0-unstable-2024-12-07";
  src = fetchFromGitHub {
    owner = "mstoeckl";
    repo = "swaylock-plugin";
    # rev = "fdade0d37707f75fc59c8c933d4b7ff029e1c3f3";
    # hash = "sha256-tLDnioRW6A08ppYGRyUdy4bnqtYiCxvT7nFBpGdCbU8=";
    rev = "7df380e37485ee173b5743200977ea292d5d538e";
    hash = "sha256-wkHd14dcc3uapDOk59ZStmqguBz/eju+O17z83v+rIU=";
  };

  strictDeps = true;
  depsBuildBuild = [ pkg-config ];
  nativeInstallCheckInputs = [
    versionCheckHook
  ];
  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    scdoc
    wayland-scanner
  ];
  buildInputs = [
    cairo
    libxcrypt
    gdk-pixbuf
    libxkbcommon
    pam
    wayland
    wayland-protocols
    systemd
  ];

  mesonFlags = [
    "-Dpam=enabled"
    "-Dgdk-pixbuf=enabled"
    "-Dman-pages=enabled"
  ];

  passthru = {
    updateScript = nix-update-script { };
  };

  meta = {
    description = "Screen locker for Wayland, forked from swaylock";
    longDescription = ''
      swaylock-pulgins is a fork of swaylock, a screen locking utility for Wayland compositors.
      On top of the usual swaylock features, it allow you to use a
      subcommand to generate the lockscreen background.

      Important note: You need to set "security.pam.services.swaylock-plugin = {};" manually.
    '';
    homepage = "https://github.com/mstoeckl/swaylock-plugin";
    mainProgram = "swaylock-plugin";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    maintainers = with lib.maintainers; [ picnoir ];
  };
})
