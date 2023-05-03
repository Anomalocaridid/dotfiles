{ pkgs, ... }: {
  programs.skim = {
    enable = true;
    defaultOptions = [
      "--color=matched:#d7d7d5,matched_bg:#ea00d9,current_bg:#091833,current_match_bg:#711c91,info:#0abdc6,border:#ea00d9,prompt:#ea00d9,pointer:#ea00d9,spinner:#00ff00"
    ];
  };

  # Use skim as a drop-in replacement for fzf
  home.packages = with pkgs; [
    (writeShellApplication {
      name = "fzf";
      runtimeInputs = [ skim ];
      text = #shell 
        ''
          ${pkgs.skim}/bin/sk "$@"
        '';
    })
  ];
}
