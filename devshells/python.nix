{ pkgs, ... }:
{
  packages = with pkgs; [
    (python3.withPackages (
      ps:
      with ps;
      lib.concatLists [
        [
          pyls-isort # Import sorter
          pylsp-mypy # Static type checker
          pytest # Needed for exercism tests
          python-lsp-black # Formatter
          python-lsp-server
        ]
        python-lsp-server.optional-dependencies.flake8 # Style checker
        python-lsp-server.optional-dependencies.mccabe # Complexity checker
        python-lsp-server.optional-dependencies.pycodestyle # PEP 8 style checker
      ]
    ))
  ];
}
