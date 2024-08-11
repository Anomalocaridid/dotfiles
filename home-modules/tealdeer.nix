{ ... }:
{
  programs.tealdeer = {
    enable = true;
    settings = {
      display = {
        use_pager = true;
        compact = true;
      };
      updates.auto_update = true;
    };
  };
}
