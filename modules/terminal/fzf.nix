{
  flake.modules.homeManager.fzf.programs.fzf = {
    enable = true;
    defaultOptions = [ ''--preview 'bat --plain --color=always "{}"' '' ];
  };
}
