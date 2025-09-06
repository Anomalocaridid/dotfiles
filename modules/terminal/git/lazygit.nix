{
  unify.modules.general.home = {
    programs.lazygit = {
      enable = true;
      settings = {
        theme.nerdFontsVersion = 3;
        update.method = false;
        disableStartupPopups = true;
        git =
          let
            logCmd = "git log --color=always";
          in
          {
            paging = {
              colorArg = "always";
              pager = ''DELTA_FEATURES="+" delta --paging=never'';
            };
            branchLogCmd = "${logCmd} {{branchName}}";
            allBranchesLogCmds = [ "${logCmd} --all" ];
          };
      };
    };

    home.shellAliases.lg = "lazygit";
  };
}
