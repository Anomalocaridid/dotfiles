{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    # Ascii Terminal greeting. 
    # Shows Linux distro and version in rainbow ascii art.

    initExtra = #shell
      ''    
      echo -en "\e[1m"
      lsb_release --description --short | 
        tr --delete '"' |
        toilet --termwidth --font smslant --filter border --directory ${pkgs.figlet}/share/figlet |
        lolcat
      echo -e "\e[1m Welcome back, $USER!\e[0m" | lolcat
      '';
  };

  home.packages = with pkgs; [
    # Needed for greeting
    toilet
    lolcat
    lsb-release
    figlet # provides fonts for toilet
  ];
}
