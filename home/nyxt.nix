{ pkgs, inputs, ... }: {
  home.packages = with pkgs; [ nyxt ];
  xdg = {
    configFile."nyxt".source = ./.config/nyxt;
    dataFile =
      let
        extensionDir = "nyxt/extensions/";
      in
      {
        "${extensionDir}nx-freestance-handler".source = inputs.nx-freestance-handler;
        "${extensionDir}nx-fruit".source = inputs.nx-fruit;
        "${extensionDir}nx-kaomoji".source = inputs.nx-kaomoji;
        "${extensionDir}nx-search-engines".source = inputs.nx-search-engines;
      };
  };
}
