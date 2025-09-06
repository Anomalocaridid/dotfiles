{
  perSystem =
    { pkgs, ... }:
    {
      devshells.ruby.packages = with pkgs; [
        ruby
        rubyPackages.minitest # needed for exercism tests
        rubyPackages.solargraph # ruby language server
      ];
    };

  unify.modules.general.home = {
    programs.helix.languages = {
      language-server.solargraph.config = {
        diagnostics = true;
        formatting = true;
      };
      languages = [
        {
          name = "ruby";
          auto-format = true;
        }
      ];
    };
  };
}
