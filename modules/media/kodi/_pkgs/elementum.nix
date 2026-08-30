{
  stdenv,
  lib,
  autoPatchelfHook,
  buildKodiAddon,
  addonDir,
  kodi,
  fetchurl,
  unzip,
  kodi-six,
  requests,
}:
let
  python = kodi.pythonPackages.python.withPackages (p: with p; [ flake8 ]);
  os = "${stdenv.hostPlatform.system}";
  osname =
    {
      x86_64-linux = "linux_x64";
      aarch64-linux = "linux_arm64";
    }
    ."${os}" or (throw "Unsupported system: ${os}");
  hash =
    {
      x86_64-linux = "0lkfm60x679dn0l20nj5dk33p1fkwdgbx5xfpkcl1c2gqs2appzm";
      aarch64-linux = "0gfn7ialjvzjnyiba8yiznck3723ykrylb8nkxkgi9bgcjjl1l0";
    }
    ."${os}" or (throw "Unsupported system: ${os}");

in
buildKodiAddon rec {
  pname = "elementum";
  namespace = "plugin.video.elementum";
  version = "0.1.114";

  src = fetchurl {
    url = "https://github.com/elgatito/plugin.video.elementum/releases/download/v${version}/plugin.video.elementum-${version}.${osname}.zip";
    sha256 = hash;
  };

  nativeBuildInputs = [
    autoPatchelfHook
    python
    unzip
  ];

  propagatedBuildInputs = [
    kodi-six
    requests
  ];

  buildInputs = [
    stdenv.cc.cc.lib
  ];

  dontBuild = true;

  installPhase = ''
    mkdir -p $out${addonDir}/${namespace}/
    cp -r ./ $out${addonDir}/${namespace}/
  '';

  meta = {
    homepage = "https://elementum.surge.sh/";
    description = "Torrenting addon for Kodi";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ anomalocaris ];
    teams = lib.teams.kodi;
  };
}
