let BarConfig =
      https://raw.githubusercontent.com/Anomalocaridid/dhall-waybar/main/BarConfig.dhall

let modules =
      https://raw.githubusercontent.com/Anomalocaridid/dhall-waybar/main/modules.dhall

let types =
      https://raw.githubusercontent.com/Anomalocaridid/dhall-waybar/main/types.dhall

let termCmd = "wezterm start -- "

let scriptDir = "~/.config/waybar/scripts/"

let modules-left =
      Some
        [ "custom/launcher"
        , "custom/right-arrow-launcher"
        , "sway/workspaces"
        , "sway/mode"
        , "custom/right-arrow-workspaces"
        , "custom/lutris"
        , "custom/right-arrow-shortcuts"
        ]

let modules-center =
      Some
        [ "custom/left-arrow-system"
        , "network"
        , "memory"
        , "cpu"
        , "disk"
        , "temperature"
        , "custom/pacman"
        , "custom/right-arrow-system"
        ]

let modules-right =
      Some
        [ "custom/left-arrow-settings"
        , "idle_inhibitor"
        , "pulseaudio"
        , "custom/keyboard-layout"
        , "battery"
        , "backlight#icon"
        , "backlight#value"
        , "custom/left-arrow-clock"
        , "clock"
        , "custom/left-arrow-tray"
        , "tray"
        , "custom/left-arrow-power"
        , "custom/weather"
        , "custom/power"
        ]

let global =
      BarConfig::{
      , layer = Some types.Layer.bottom
      , position = Some types.Position.top
      , height = Some 30
      , width = Some 10
      , modules-left
      , modules-center
      , modules-right
      }

let idle_inhibitor =
      modules.IdleInhibitor::{
      , format = Some "{icon} "
      , format-icons = Some
          (types.Icons.Object (toMap { activated = "", deactivated = "" }))
      }

let battery =
      modules.Battery::{
      , states = Some (toMap { warning = 30, critical = 15 })
      , format = Some "{capacity}% {icon}"
      , format-charging = Some "{capacity}% "
      , format-plugged = Some "{capacity}% "
      , format-icons = Some (types.Icons.Array [ "", "", "", "", "" ])
      }

let custom/PBPbattery =
      modules.Custom::{ exec = Some (scriptDir ++ "PBPbattery.sh") }

let clock =
      modules.Clock::{
      , interval = Some (types.Interval.Int 1)
      , format-alt = Some " {:%a, %b %d, %Y}"
      , format = Some " {:%r}"
      , tooltip-format = Some
          ''
          {:%A, %B %e, %Y
          %R:%S}''
      , on-click-right = Some "yad --calendar --no-buttons"
      }

let cpu =
      modules.CPU::{
      , interval = Some (types.Interval.Int 5)
      , format = Some " {usage}%"
      , states = Some (toMap { warning = 70, critical = 90 })
      , on-click = Some (termCmd ++ "bpytop")
      }

let custom/keyboard-layout =
      modules.Custom::{
      , exec = Some
          "swaymsg -t get_inputs | grep -m1 'xkb_active_layout_name' | cut -d '\"' -f4"
      , interval = Some (types.Interval.Int 30)
      , format = Some "  {}"
      , signal = Some 1
      , tooltip = Some False
      , on-click = Some (scriptDir ++ "keyhint.sh")
      }

let custom/pacman =
      modules.Custom::{
      , format = Some " {}"
      , interval = Some (types.Interval.Int 3600)
      , exec = Some "checkupdates | wc -l"
      , on-click = Some "wezterm start -- paru; pkill -SIGRTMIN+8 waybar"
      , signal = Some 8
      }

let disk =
      modules.Disk::{
      , interval = cpu.interval
      , format = Some " {}%"
      , states = cpu.states
      , on-click = Some (termCmd ++ "zsh -c 'dust | bat --paging always'")
      }

let memory =
      modules.Memory::{
      , interval = cpu.interval
      , format = Some " {}%"
      , states = cpu.states
      , on-click = cpu.on-click
      }

let networkFormat1 = "{essid} ({signalStrength}%)"

let networkFormat2 = "{ifname}: {ipaddr}/{cidr}"

let network =
      modules.Network::{
      , interval = cpu.interval
      , format-wifi = Some ("  " ++ networkFormat1)
      , format-ethernet = Some ("  " ++ networkFormat2)
      , format-disconnected = Some "⚠  Disconnected"
      , tooltip-format = Some "{ifname}: {ipaddr}"
      , on-click = Some (termCmd ++ "nmtui")
      }

let `network#vpn` =
      modules.Network::{
      , interface = Some "tun0"
      , format = Some ("" ++ networkFormat1)
      , format-disconnected = network.format-disconnected
      , tooltip-format = Some networkFormat2
      }

let sway/mode = modules.Sway.Mode::{ tooltip = Some False }

let sway/window = modules.Sway.Window::{ max-length = Some 120 }

let sway/workspaces =
      modules.Sway.Workspaces::{
      , disable-scroll = Some True
      , all-outputs = Some False
      , format = Some " {icon} "
      }

let temperature =
      modules.Temperature::{
      , critical-threshold = Some 75
      , format = Some " {temperatureC}°C"
      }

let volumeFormat = "{volume}% "

let pulseaudioFormat = volumeFormat ++ "{icon}"

let bluetoothFormat = "{icon}  {format_source}"

let pulseaudio =
      modules.Pulseaudio::{
      , scroll-step = Some 1.0
      , format = Some pulseaudioFormat
      , format-bluetooth = Some (volumeFormat ++ bluetoothFormat)
      , format-bluetooth-muted = Some (" " ++ bluetoothFormat)
      , format-muted = Some "婢 {format_source}"
      , format-source = Some (volumeFormat ++ "")
      , format-source-muted = Some ""
      , format-icons = Some
          ( types.Icons.Object
              ( toMap
                  { headphone = ""
                  , hands-free = "וֹ"
                  , headset = "  "
                  , phone = ""
                  , portable = ""
                  , car = ""
                  , default = ""
                  }
              )
          )
      , on-click = Some "pavucontrol"
      , on-scroll-up = Some "pamixer -ui 2"
      , on-scroll-down = Some "pamixer -ud 2"
      }

let custom/weather =
      modules.Custom::{
      , exec = Some (scriptDir ++ "weather.sh")
      , format = Some "{text} {icon}"
      , return-type = Some types.ReturnType.json
      , interval = Some (types.Interval.Int 600)
      , tooltip-format = Some "{tooltip}"
      }

let tray = modules.Tray::{ icon-size = Some 18, spacing = Some 10 }

let `backlight#icon` =
      modules.Backlight::{
      , format = Some "{icon}"
      , format-icons = Some (types.Icons.Array [ "" ])
      , on-scroll-down = Some "brightnessctl -c backlight set 5%-"
      , on-scroll-up = Some "brightnessctl -c backlight set +5%"
      }

let `backlight#value` =
      modules.Backlight::{
      , format = Some "{percent}%"
      , on-scroll-down = `backlight#icon`.on-scroll-down
      , on-scroll-up = `backlight#icon`.on-scroll-up
      }

let custom/lutris =
      modules.Custom::{
      , format = Some " "
      , on-click = Some "exec lutris"
      , tooltip = Some False
      }

let custom/launcher =
      modules.Custom::{
      , format = Some " "
      , on-click = Some "exec wofi -c ~/.config/wofi/config -I"
      , tooltip = Some False
      }

let custom/power =
      modules.Custom::{
      , format = Some "⏻"
      , on-click = Some ("exec " ++ scriptDir ++ "power-menu.sh")
      , tooltip = Some False
      }

let right-arrow = modules.Custom::{ format = Some "", tooltip = Some False }

let left-arrow = modules.Custom::{ format = Some "", tooltip = Some False }

let arrows =
      { custom/right-arrow-launcher = right-arrow
      , custom/right-arrow-workspaces = right-arrow
      , custom/right-arrow-shortcuts = right-arrow
      , custom/left-arrow-system = left-arrow
      , custom/right-arrow-system = right-arrow
      , custom/left-arrow-settings = left-arrow
      , custom/left-arrow-clock = left-arrow
      , custom/left-arrow-tray = left-arrow
      , custom/left-arrow-power = left-arrow
      }

let modules =
          { idle_inhibitor
          , battery
          , custom/PBPbattery
          , clock
          , cpu
          , custom/keyboard-layout
          , custom/pacman
          , disk
          , memory
          , network
          , `network#vpn`
          , sway/mode
          , sway/window
          , sway/workspaces
          , temperature
          , pulseaudio
          , custom/weather
          , tray
          , `backlight#icon`
          , `backlight#value`
          , custom/lutris
          , custom/launcher
          , custom/power
          }
      /\  arrows

in  global /\ modules
