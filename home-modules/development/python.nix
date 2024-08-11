{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (python3.withPackages (
      ps: with ps; [
        pytest # Needed for exercism tests
      ]
    ))
  ];

  programs.helix = {
    extraPackages = with pkgs; [
      (python3.withPackages (
        ps:
        with ps;
        lib.concatLists [
          [
            pylsp-mypy # Static type checker
            # Commented out because it is broken
            # python-lsp-black # Formatter
            python-lsp-server
            pyls-isort # Import sorter
          ]
          python-lsp-server.optional-dependencies.flake8 # Style checker
          python-lsp-server.optional-dependencies.mccabe # Complexity checker
          python-lsp-server.optional-dependencies.pycodestyle # PEP 8 style checker
        ]
      ))
    ];

    languages.language = [
      {
        name = "python";
        auto-format = true;
      }
    ];
  };
}
