{ pkgs, ... }: {
  home.packages = with pkgs; [ webcord ];
  # TODO: use fufexan/webcord-flake when it updates for saner theme configuration
}
