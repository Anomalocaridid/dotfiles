{ config, ... }:
{
  flake.meta.gitHubUsername = "Anomalocaridid";

  unify.modules.general.home.programs = {
    git = {
      enable = true;
      settings = {
        user = {
          email = "29845794+Anomalocaridid@users.noreply.github.com";
          name = config.flake.meta.gitHubUsername;
        };
        init.defaultBranch = "main";
        core.autocrlf = "input";
        merge.conflictstyle = "diff3";
        diff.colorMoved = "default";
        fetch.prune = true;
      };
    };

    # Syntax-aware git merge driver
    mergiraf.enable = true;
  };
}
