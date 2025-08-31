{ lib, ... }:
{
  # This is used to make certain pieces on information globally available
  # Individual options are not defined in order to reduce duplicate logic
  options.flake.meta = lib.mkOption {
    type = lib.types.anything;
  };
}
