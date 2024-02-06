{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      julia
    ];
    file = {
      ".julia/config/startup.jl".source = ../.julia/config/startup.jl;
    };
  };

  programs.helix.languages.language = [
    {
      name = "julia";
      auto-format = true;
    }
  ];
}
