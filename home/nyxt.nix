{ pkgs, inputs, ... }: {
  home.packages = with pkgs; [ nyxt ];
  xdg = {
    configFile."nyxt".source = ./.config/nyxt;
    dataFile =
      let
        extensionDir = "nyxt/extensions/";
      in
      {
        "${extensionDir}nx-fruit".source = inputs.nx-fruit;
        "${extensionDir}nx-search-engines".source = inputs.nx-search-engines;
        "${extensionDir}nx-router".source = inputs.nx-router;
      };
  };
}
