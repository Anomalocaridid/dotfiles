{
  perSystem =
    { pkgs, ... }:
    {
      devshells.lua.packages = with pkgs; [
        # TODO: Change when Lua 5.4 becomes the default version
        (lua5_4.withPackages (
          ps: with ps; [
            busted # Needed for exercism tests
          ]
        ))
        lua-language-server
      ];
    };
}
