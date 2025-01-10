{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (ignis.overrideAttrs (oldAttrs: {
      propagatedBuildInputs =
        (oldAttrs.propagatedBuildInputs or [ ])
        ++ (with pkgs; [
          (python312.withPackages (
            ps: with ps; [
              psutil
            ]
          ))
        ]);
    }))
  ];

  xdg.configFile.ignis.source = ./.config/ignis;
}
