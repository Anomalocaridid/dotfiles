{ config, lib, pkgs, ... }: {
  home.packages = with pkgs;
    let
      nix_pkgs = [
        nil
        nixpkgs-fmt
      ];
      python_pkgs = [
        (python3.withPackages
          (ps: with ps; lib.concatLists [
            [
              pylsp-mypy
              pytest
              python-lsp-black
              python-lsp-server
            ]
            python-lsp-server.optional-dependencies.flake8
            python-lsp-server.optional-dependencies.mccabe
            python-lsp-server.optional-dependencies.pycodestyle
          ]))
      ];
      haskell_pkgs = [
        cabal-install
        ghc
        haskell-language-server
        hlint
        ormolu
        stack
      ];
      bash_pkgs = [
        bats
        nodePackages.bash-language-server
        shellcheck
        shfmt
      ];
      lisp_pkgs = [
        sbcl
        gcc # Required to compile linedit
      ];
      rust_pkgs = [
        cargo
        rustc
        rust-analyzer
      ];
      c_pkgs = [
        bear # For creating compilation databases for clangd
        clang-tools # Provides clangd
      ];
      lua_pkgs = [
        lua
        lua-language-server
      ];
      julia_pkgs = [
        julia
      ];
      markdown_pkgs = [
        marksman
      ];
      unison_pkgs = [
        unison-ucm
        netcat-gnu # required for lsp
      ];
      ocaml_pkgs = [
        dune_3
        ocamlformat
        dune-release
      ] ++ (with ocamlPackages; [
        ocaml-lsp
        odoc
        utop
        ounit # required for exercism tests
      ]);
      clojure_pkgs = [
        clojure
        clojure-lsp
        leiningen
      ];
    in
    lib.concatLists [
      nix_pkgs
      python_pkgs
      haskell_pkgs
      bash_pkgs
      lisp_pkgs
      rust_pkgs
      c_pkgs
      lua_pkgs
      julia_pkgs
      markdown_pkgs
      unison_pkgs
      ocaml_pkgs
      clojure_pkgs
    ];
  home.file = {
    ".ghci".text = ":set prompt \"\\ESC[1;35mλ> \\ESC[m\"";
    ".sbclrc".text = #lisp
      ''
        ;;; The following lines added by ql:add-to-init-file:
        #-quicklisp
        (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                               (user-homedir-pathname))))
          (when (probe-file quicklisp-init)
            (load quicklisp-init)))

        ;;; Check for --no-linedit command-line option.
        (if (member "--no-linedit" sb-ext:*posix-argv* :test 'equal)
            (setf sb-ext:*posix-argv*
                  (remove "--no-linedit" sb-ext:*posix-argv* :test 'equal))
            (when (interactive-stream-p *terminal-io*)
              (require :sb-aclrepl)
              (require :linedit)
              (funcall (intern "INSTALL-REPL" :linedit) :wrap-current t)))
      '';
    ".clang-format".source =
      let
        yamlFormat = pkgs.formats.yaml { };
      in
      yamlFormat.generate "clang-format" {
        BasedOnStyle = "LLVM";
        IndentWidth = 4;
        IndentCaseLabels = true;
        AlignConsecutiveDeclarations = true;
      };
    ".julia/config/startup.jl".source = ../dotfiles/.julia/config/startup.jl;
  };

  systemd.user.services.quicklisp-setup = {
    Unit = {
      Description = "Ensure Quicklisp is set up for sbcl";
      ConditionPathExists = "!${config.home.homeDirectory}/quicklisp/setup.lisp";
    };
    Service = {
      Type = "oneshot";
      ExecStartPre = "${pkgs.curl}/bin/curl https://beta.quicklisp.org/quicklisp.lisp --output /tmp/quicklisp.lisp";
      ExecStart = ''
        ${pkgs.sbcl}/bin/sbcl --no-userinit --non-interactive --load /tmp/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:quickload "linedit")'
      '';
      # Sometimes fails on first try, but succeeds on second
      # Possibly some issue with downloading stuff
      Restart = "on-failure";
    };
    Install.WantedBy = [ "default.target" ];
  };
}
