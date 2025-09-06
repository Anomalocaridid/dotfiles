{
  unify.modules.general.nixos =
    { pkgs, ... }:
    {
      security.sudo = {
        package = pkgs.sudo.override { withInsults = true; };
        extraConfig = # sudo
          ''
            # Prevents sudo lecture from appearing after reboot without persisting
            Defaults lecture = never
            # Sudo insults after failed attempts because why not
            Defaults insults
          '';
      };
    };
}
