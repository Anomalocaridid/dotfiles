{
  unify.modules.general.nixos =
    { lib, pkgs, ... }:
    {
      # Just in case, disable userdb, which can potentially store PII
      systemd.package = pkgs.systemd.override { withUserDb = false; };

      # This is already off by default, but hey, what if something at
      # some point turns it on in order to do a surveillance?
      services.userdbd.enable = lib.mkForce false;
    };
}
