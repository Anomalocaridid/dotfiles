{ pkgs, ... }:
{
  packages = with pkgs; [
    erlang
    gnumake # Required for exercism tests
    lfe
    rebar3 # Erlang build tool and test runner
  ];
}
