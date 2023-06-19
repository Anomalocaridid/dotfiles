{ lib, pkgs, ... }: {
  # Weechat service configured in configuration.nix
  home.packages = with pkgs; [ weechat ];
  # Individually link files because sec.conf can't be persisted otherwise
  xdg.configFile =
    let
      configDir = ../dotfiles/.config/weechat;
    in
    lib.foldr
      (file: acc:
        {
          "weechat/${file}".source = configDir + "/${file}";
        } // acc
      )
      { }
      (builtins.attrNames (builtins.readDir configDir));
}
