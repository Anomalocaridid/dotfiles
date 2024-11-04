{ pkgs, ... }:
{
  packages = with pkgs; [
    elvis-erlang # Erlang style reviewer
    erlang
    erlang-ls # Erlang language server
    rebar3 # Erlang build tool and test runner
  ];
}
