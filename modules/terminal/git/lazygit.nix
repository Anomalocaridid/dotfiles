{
  flake.modules.homeManager.general = {
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
            diffRenderers = [
              {
                colorArg = "always";
                command = ''DELTA_FEATURES="+" delta --paging=never'';
              }
            ];
            branchLogCmd = "${logCmd} {{branchName}}";
            allBranchesLogCmds = [ "${logCmd} --all" ];
          };
      };
    };

    home.shellAliases.lg = "lazygit";
  };
}
