{ config, ... }:
{
  flake.meta.gitHubUsername = "Anomalocaridid";

  unify.modules.general.home.programs.git = {
    enable = true;
    userEmail = "29845794+Anomalocaridid@users.noreply.github.com";
    userName = config.flake.meta.gitHubUsername;
    extraConfig = {
      init.defaultBranch = "main";
      core.autocrlf = "input";
      merge.conflictstyle = "diff3";
      diff.colorMoved = "default";
    };
  };
}
