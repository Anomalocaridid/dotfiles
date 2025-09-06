{
  perSystem =
    { pkgs, ... }:
    {
      devshells.jq.packages = with pkgs; [
        bats # Needed for exercism tests
        jq
        jq-lsp
      ];
    };
}
