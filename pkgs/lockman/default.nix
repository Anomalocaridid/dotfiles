{
  asciiquarium-transparent,
  cbonsai,
  coreutils,
  ghostty,
  handlr-regex,
  hyprland,
  hyprlock,
  fastfetch,
  pipes-rs,
  pv,
  ternimal,
  unimatrix,
  util-linux,
  writeShellApplication,
  ...
}:
let
  scriptName = "lockman";
  # Workaround to pipe neofetch's output in a loop
  fetch-wrapper = writeShellApplication {
    name = "fetch-wrapper.sh";
    runtimeInputs = [
      fastfetch
      pv
      util-linux # provides script
    ];
    text = ''
      while true; do
        # Script makes fastfetch think it is outputting to a terminal
        # Necessary to preserve colors
        # Also make sure to not write output to a file by outputting to /dev/null
        script --quiet --log-out /dev/null  --command fastfetch | pv -qL 200
      done
    '';
  };
  # Workaround to give ternimal terminal dimensions
  ternimal-wrapper = writeShellApplication {
    name = "ternimal-wrapper.sh";
    runtimeInputs = [ ternimal ];
    text = ''
      ternimal width="$(tput cols)" height=$(($(tput lines) * 2))
    '';
  };
  # Randomly picks a screensaver
  pick-screensaver = writeShellApplication {
    name = "pick-screensaver.sh";
    runtimeInputs = [
      asciiquarium-transparent
      cbonsai
      fetch-wrapper # Workaround to create effect for fetch program
      pipes-rs
      ternimal-wrapper # Workaround to give ternimal terminal dimensions
      unimatrix
    ];
    text = ''
      readonly SCREENSAVERS=(
        "asciiquarium --transparent"
        "cbonsai --live --infinite"
        "fetch-wrapper.sh"
        "pipes-rs"
        "unimatrix --asynchronous --flashers"
        "ternimal-wrapper.sh"
      )
      # Give terminal a bit of time to properly set dimensions
      sleep 0.2
      ''${SCREENSAVERS[(($RANDOM % ''${#SCREENSAVERS[@]}))]}
    '';
  };
in
writeShellApplication {
  name = "${scriptName}.sh";
  runtimeInputs = [
    coreutils # provides sleep
    ghostty
    handlr-regex
    hyprland # provides hyprctl
    hyprlock
    util-linux # provides flock
    pick-screensaver # Randomly picks a screensaver
  ];
  text =
    let
      # Because ghostty uses gtk to set the class, it has to look like a domain
      scriptClass = "com.terminal.${scriptName}";
    in
    # bash
    ''
      # Exit if script is already running (lock exists)
      exec 3>/tmp/${scriptName}.lock
      flock --nonblock 3
      # Move to empty workspace
      hyprctl dispatch workspace empty
      # Ensure screensaver will be fullscreen
      hyprctl keyword windowrulev2 'fullscreen, class:${scriptClass}'
      # Run screensaver
      handlr launch x-scheme-handler/terminal -- --class=${scriptClass} -e pick-screensaver.sh
      # Focus screensaver (assumed to be already fullscreened)
      hyprctl dispatch focuswindow class:"${scriptClass}"
      # Turn on CRT shader
      hyprctl keyword decoration:screen_shader ${../../assets/crt.frag};
      # Lock screen (blocks until unlocked)
      hyprlock
      # Close screensaver, return to original workspace, remove fullscreen rule, and turn off shader
      hyprctl --batch "dispatch closewindow class:${scriptClass}; dispatch workspace previous; reload"
      # Release lock
      echo "$$" >&3
    '';
}
