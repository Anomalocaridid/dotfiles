{ ... }:
{
  programs.zoxide = {
    enable = true;
    options = [
      # Alias as cd
      "--cmd cd"
    ];
  };
}
