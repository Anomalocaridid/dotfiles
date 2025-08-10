{
  flake.modules.homeManager.zoxide.programs.zoxide = {
    enable = true;
    # Alias as cd
    options = [ "--cmd cd" ];
  };
}
