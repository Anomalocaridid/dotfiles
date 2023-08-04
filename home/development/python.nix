{ pkgs, ... }: {
  home.packages = with pkgs; [
    tio # needed for KMK debugging
    circup # Update circuitpython libraries
    micropython # Includes mpy-cross for KMK
    (python3.withPackages
      (ps: with ps; lib.concatLists [
        [
          pylsp-mypy # static type checker
          pytest # test framework/runner
          python-lsp-black # formatter
          python-lsp-server
        ]
        python-lsp-server.optional-dependencies.flake8 # style checker
        python-lsp-server.optional-dependencies.mccabe # complexity checker
        python-lsp-server.optional-dependencies.pycodestyle # PEP 8 style checker
      ]))
  ];
}
