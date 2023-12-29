{ writeShellApplication
, coreutils
, hyprland
, swaylock-effects
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
    hyprland # provides hyprctl
    swaylock-effects
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
  text = ''
    # Get paths of commands or else terminal command will not see them
    readonly SCREENSAVERS=(
      "$(which asciiquarium) --transparent"
      "$(which cbonsai) --live --infinite"
      "$(which loop.sh) $(which neofetch-wrapper.sh)"
      "$(which pipes-rs)"
      "$(which unimatrix) --asynchronous --flashers"
    )
    # Exit if script is already running (lock exists)
    exec 3>/tmp/${scriptName}.lock
    flock --nonblock 3
    # Move to empty workspace
    hyprctl dispatch workspace empty
    # Run screensaver (requires splitting string)
    # Need to start as new process or else window rules in command will not be applied
    # shellcheck disable=SC2086
    hyprctl dispatch exec "[fullscreen]" -- wezterm start --always-new-process --class=${scriptName} -- ''${SCREENSAVERS[(($RANDOM % ''${#SCREENSAVERS[@]}))]}
    # Focus screensaver
    hyprctl dispatch focuswindow "^(${scriptName})$"
    # Turn on CRT shader
    hyprctl keyword decoration:screen_shader ${../../assets/crt.frag};
    # Lock screen (blocks until unlocked)
    swaylock
    # Close screensaver, return to original workspace, and turn off shader
    hyprctl --batch "dispatch closewindow ^(${scriptName})$; \
                     dispatch workspace previous; \
                     reload"
    # Release lock
    echo "$$" >&3
  '';
}
