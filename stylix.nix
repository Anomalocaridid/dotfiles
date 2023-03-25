{ pkgs, ... }:
{
  image = pkgs.fetchurl {
    url = "https://www.pixelstalk.net/wp-content/uploads/images1/Game-OutRun-Photos-Download.jpg"
    sha256 = "";
  };
  base16Scheme = {
    slug = "cyberpunk-neon";
    scheme = "Cyberpunk Neon";
    author = "Roboron3042";
    base00 = "000b1e";
    base01 = "123e7c";
    base02 = "1c61c2";
    base03 = "056472";
    base04 = "00000a";
    base05 = "0abdc6";
    base06 = "d7d7d5";
    base07 = "d7d7d5";
    base08 = "ff0000";
    base09 = "f57800";
    base0A = "f57800";
    base0B = "d300c4";
    base0C = "0abdc6";
    base0D = "00ff00";
    base0E = "711c91";
    base0F = "e62800";
  };

  fonts = {
    monospace = {
      package = ( pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; } );
      name = "Fira Code Nerd Font";
    };
  };
}
