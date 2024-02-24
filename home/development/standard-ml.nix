{ pkgs, ... }: {
  home.packages = with pkgs; [
    polyml
  ];

  programs.helix.languages.language = [
    {
      name = "sml";
      auto-format = true;
      formatter.command = "${pkgs.smlfmt}/bin/smlfmt";
      indent = { tab-width = 2; unit = "  "; };
    }
  ];
}
