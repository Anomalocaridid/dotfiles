{
  flake.modules = {
    nixos.laptop.services.upower.enable = true;
    homeManager.laptop.services.poweralertd.enable = true;
  };
}
