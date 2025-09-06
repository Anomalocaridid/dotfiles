{
  perSystem =
    { pkgs, ... }:
    {
      devshells.lua.packages = with pkgs; [
        # TODO: Change when Lua 5.4 becomes the default version
        (lua5_4.withPackages (
          # Needed for exercism tests
          ps: with ps; [ busted ]
        ))
        lua-language-server
      ];
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "lua";
      auto-format = true;
    }
  ];
}
