{
  unify.modules.general.home = {
    programs.git.delta = {
      enable = true;
      options = {
        navigate = true;
        interactive.keep-plus-minus-markers = false;
      };
    };

    home.sessionVariables = {
      # Ensure bat's line numbers don't show up and mess things up
      DELTA_PAGER = "bat --plain";
      # Ensure --side-by-side is only used for `git diff`
      DELTA_FEATURES = "+side-by-side";
    };
  };
}
