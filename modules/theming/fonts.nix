{
  unify.modules.general = {
    nixos =
      { pkgs, ... }:
      {
        fonts = {
          packages = with pkgs; [ nerd-fonts.fira-code ];

          fontconfig = {
            defaultFonts = {
              serif = [ "FiraCode Nerd Font Propo" ];
              sansSerif = [ "FiraCode Nerd Font Propo" ];
              monospace = [ "FiraCode Nerd Font" ];
            };
            localConf = # xml
              ''
                <match target="font">
                  <test name="family" compare="contains">
                    <string>FiraCode Nerd Font</string>
                  </test>
                  <edit name="fontfeatures" mode="append">
                    <string>ss09</string> <!-- >>= <<= ||= |= -->
                    <string>cv25</string> <!-- .- -->
                    <string>cv26</string> <!-- :- -->
                    <string>cv32</string> <!-- .= -->
                    <string>cv27</string> <!-- [] -->
                    <string>cv28</string> <!-- {. .} -->
                    <string>ss06</string> <!-- \\ -->
                    <string>ss07</string> <!-- =~ !~ -->
                  </edit>
                </match>
              '';
          };
        };
      };

    home =
      { osConfig, ... }:
      {
        fonts.fontconfig.defaultFonts = osConfig.fonts.fontconfig.defaultFonts;
      };
  };
}
