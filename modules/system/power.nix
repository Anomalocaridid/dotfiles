{
  unify.modules.laptop = {
    nixos = {
      services = {
        upower.enable = true;
        auto-cpufreq.enable = true;
      };
    };

    home = {
      services.poweralertd.enable = true;
    };
  };
}
