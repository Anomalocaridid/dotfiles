{ ... }: {
  programs.fzf = {
    enable = true;
    catppuccin.enable = true;
    defaultOptions = [ ''--preview 'bat --plain --color=always "{}"' '' ];
  };
}




