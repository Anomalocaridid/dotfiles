{ writeShellApplication
, coreutils
, handlr-regex
, hyprland
, hyprlock
, util-linux
, wezterm
, neofetch
, pv
, asciiquarium-transparent
, cbonsai
, pipes-rs
, unimatrix
, ...
}:
let
  scriptName = "lockman";
  # Workaround to infinitely loop programs
  loop = writeShellApplication {
    name = "loop.sh";
    text = ''
      while true; do
        "$@"
      done
    '';
  };
  # Workaround to pipe neofetch's output
  neofetch-wrapper = writeShellApplication {
    name = "neofetch-wrapper.sh";
    runtimeInputs = [ pv neofetch ];
    text = ''
      neofetch | pv -qL 200
    '';
  };
in
writeShellApplication {
  name = "${scriptName}.sh";
  runtimeInputs = [
    coreutils # provides sleep
    handlr-regex
    hyprland # provides hyprctl
    hyprlock
    util-linux # provides flock
    wezterm
    loop # Workaround to infinitely loop programs
    # screensavers
    asciiquarium-transparent
    cbonsai
    neofetch-wrapper # Workaround to pipe neofetch's output
    pipes-rs
    unimatrix
  ];
  text =
    let
      scriptClass = "^(${scriptName})$";
    in
    # bash
    ''
      readonly SCREENSAVERS=(
        "asciiquarium --transparent"
        "cbonsai --live --infinite"
        "loop.sh neofetch-wrapper.sh"
        "pipes-rs"
        "unimatrix --asynchronous --flashers"
      )
      # Exit if script is already running (lock exists)
      exec 3>/tmp/${scriptName}.lock
      flock --nonblock 3
      # Move to empty workspace
      hyprctl dispatch workspace empty
      # Ensure screensaver will be fullscreen
      hyprctl keyword windowrulev2 'fullscreen, class:${scriptClass}'
      # Run screensaver (requires splitting string)
      # shellcheck disable=SC2086
      handlr launch x-scheme-handler/terminal -- --class=${scriptName} -- ''${SCREENSAVERS[(($RANDOM % ''${#SCREENSAVERS[@]}))]}
      # Focus screensaver (assumed to be already fullscreened)
      hyprctl dispatch focuswindow "${scriptClass}"
      # Turn on CRT shader
      hyprctl keyword decoration:screen_shader ${../../assets/crt.frag};
      # Lock screen (blocks until unlocked)
      hyprlock
      # Close screensaver, return to original workspace, remove fullscreen rule, and turn off shader
      hyprctl --batch "dispatch closewindow ${scriptClass}; dispatch workspace previous; reload"
      # Release lock
      echo "$$" >&3
    '';
}
