{ pkgs, ... }:
{
  packages = with pkgs; [
    roswell # Common Lisp build tool/test runner
    (sbcl.withPackages (ps: with ps; [ linedit ]))
  ];

  # Install necessary dependencies like quicklisp
  # Rather noisy, so check if they need to be installed first
  devshell.startup.roswell.text = ''
    if [[ ! -d ~/.roswell ]]; then
      ros help
    fi
  '';
}
