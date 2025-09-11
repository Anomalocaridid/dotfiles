import asyncio
import math
import shutil
from collections import Counter, defaultdict
from collections.abc import Callable
from datetime import datetime
from time import gmtime, strftime

import psutil  # Not included by default
import unicodeit  # Not included by default # pyright: ignore[reportMissingTypeStubs]
import wm  # pyright: ignore[reportMissingImports] # Custom module with constants from window manager config
from ignis import utils, widgets
from ignis.css_manager import CssInfoPath, CssManager
from ignis.services.audio import AudioService
from ignis.services.fetch import FetchService
from ignis.services.mpris import MprisPlayer, MprisService
from ignis.services.network import NetworkService, WifiDevice
from ignis.services.niri import NiriService, NiriWindow, NiriWorkspace
from ignis.services.system_tray import SystemTrayItem, SystemTrayService
from ignis.services.upower import UPowerDevice, UPowerService
from ignis.variable import Variable

css_manager = CssManager.get_default()
css_manager.apply_css(
    CssInfoPath(
        name="main",
        path=f"{utils.get_current_dir()}/style.scss",
        compiler_function=lambda path: utils.sass_compile(path=path),
        priority="user",
    )
)

audio = AudioService.get_default()
fetch = FetchService.get_default()
mpris = MprisService.get_default()
network = NetworkService.get_default()
niri = NiriService.get_default()
system_tray = SystemTrayService.get_default()
upower = UPowerService.get_default()

# Global constants
BAR_SPACING = 10
WIDGET_SPACING = 5
ICON_SPACING = 2

TERMINAL = "handlr launch x-scheme-handler/terminal --"


def toggle_variable(var: Variable):
    def _inner(_) -> None:
        var.value = not var.value

    return _inner


def toggle_window(window: widgets.Window):
    def _inner(_) -> None:
        window.visible = not window.visible

    return _inner


@utils.debounce(wm.SCROLL_COOLDOWN_MS)
def scroll_workspaces(monitor_name: str, step: int) -> None:
    current = list(
        filter(lambda w: w.is_active and w.output == monitor_name, niri.workspaces)
    )[0].idx
    niri.switch_to_workspace(min(max(current + step, 0), wm.WORKSPACES))


# TODO: Preserve ordering of windows after that information is exposed in Niri IPC
# see: https://github.com/YaLTeR/niri/pull/1265
def workspace_button(
    workspace: NiriWorkspace,
    window_counts: dict[tuple[str, bool], int],
) -> widgets.Button:
    widget = widgets.Button(
        css_classes=["flat"],
        on_click=lambda _, id=workspace.idx: niri.switch_to_workspace(id),
        child=widgets.Box(
            child=[
                widgets.Label(label=f"{workspace.idx}{':' if window_counts else ''}")
            ]
            + [
                widgets.Box(
                    child=[
                        widgets.Icon(
                            image=utils.get_app_icon_name(app_id),
                            css_classes=["focused"] if is_focused else [],
                        ),
                        # Show count in superscript
                        widgets.Label(label=unicodeit.replace(f"^{{{count}}}"))
                        if count > 1
                        else None,
                    ]
                )
                for (app_id, is_focused), count in window_counts.items()
            ]
        ),
    )

    if workspace.is_active:
        widget.add_css_class("active")

    return widget


def window_to_workspace_idx(workspaces: list[NiriWorkspace], window: NiriWindow) -> int:
    return list(filter(lambda ws: ws.id == window.workspace_id, workspaces))[0].idx


def format_workspaces(
    workspaces: list[NiriWorkspace], windows: list[NiriWindow]
) -> list[widgets.Button]:
    windows_by_workspace = defaultdict(Counter)

    for window in windows:
        # Only keep relevant information so that using windows are not counted as unique
        windows_by_workspace[window_to_workspace_idx(workspaces, window)].update(
            [(window.app_id, window.is_focused)]
        )
    return [workspace_button(ws, windows_by_workspace[ws.idx]) for ws in workspaces]


def workspaces(monitor_name: str) -> widgets.EventBox:
    # Make sure to gracefully handle niri not being available
    if niri.is_available:
        return widgets.EventBox(
            on_scroll_up=lambda _: scroll_workspaces(monitor_name, 1),
            on_scroll_down=lambda _: scroll_workspaces(monitor_name, -1),
            spacing=WIDGET_SPACING,
            # Bind to active_window also to ensure focused window is up to date
            child=niri.bind_many(
                ["workspaces", "windows"],
                transform=lambda workspaces, windows: format_workspaces(
                    workspaces, windows
                ),
            ),
        )
    else:
        return widgets.EventBox()


def active_window(monitor_name: str) -> widgets.Box:
    title = niri.bind(
        "active_window",
        transform=lambda active_window: active_window.title,
    )

    return widgets.Box(
        spacing=WIDGET_SPACING,
        visible=niri.bind(
            "active_output", lambda active_output: active_output == monitor_name
        ),
        child=[
            widgets.Icon(
                image=niri.bind(
                    "active_window",
                    transform=lambda active_window: utils.get_app_icon_name(
                        active_window.app_id
                    ),
                )
            ),
            widgets.Label(
                ellipsize="end",
                max_width_chars=15,
                label=title,
                tooltip_text=title,
            ),
        ],
    )


def track_progress(player: MprisPlayer):
    return player.bind_many(
        ["position", "length"],
        transform=lambda position,
        length: f"{strftime('%M:%S', gmtime(position))} / {strftime('%M:%S', gmtime(length))}",
    )


def mpris_title(
    player: MprisPlayer, media_player_window: widgets.Window
) -> widgets.EventBox:
    artist_revealer = widgets.Revealer(
        child=widgets.Box(
            spacing=WIDGET_SPACING,
            child=[
                widgets.Label(
                    label=player.bind("artist"),
                    ellipsize="end",
                    max_width_chars=20,
                ),
                widgets.Label(
                    # Only show if there is an artist
                    visible=player.bind(
                        "artist",
                        transform=lambda artist: artist != "",
                    ),
                    label="-",
                ),
            ],
        ),
        reveal_child=False,
        transition_type="slide_right",
    )

    return widgets.EventBox(
        spacing=WIDGET_SPACING,
        setup=lambda self: player.connect(
            "closed",
            lambda _: self.unparent(),  # remove widget when player is closed
        ),
        child=[
            widgets.Icon(image="audio-x-generic"),
            artist_revealer,
            widgets.Label(
                ellipsize="end",
                max_width_chars=20,
                label=player.bind(
                    "title",
                ),
            ),
        ],
        tooltip_text=track_progress(player),
        on_hover=lambda _: artist_revealer.set_reveal_child(True),
        on_hover_lost=lambda _: artist_revealer.set_reveal_child(False),
        on_click=toggle_window(media_player_window),
    )


def mpris_button(
    player: MprisPlayer,
    icon_name: str,
    on_click: Callable[[], asyncio.Task[None]],
    visible_when: str = "can_control",
) -> widgets.Button:
    return widgets.Button(
        css_classes=["circular"],
        visible=player.bind(visible_when),
        child=widgets.Icon(pixel_size=32, image=icon_name),
        on_click=lambda _: on_click(),
    )


def mpris_player(player: MprisPlayer) -> widgets.Box:
    return widgets.Box(
        spacing=WIDGET_SPACING,
        setup=lambda self: player.connect(
            "closed",
            lambda _: self.unparent(),  # remove widget when player is closed
        ),
        css_classes=["player"],
        child=[
            widgets.Picture(
                image=player.bind(
                    "art_url",
                    transform=lambda art_url: "applications-multimedia"
                    if art_url is None
                    else art_url,
                ),
                width=100,
                height=100,
            ),
            widgets.Box(
                spacing=WIDGET_SPACING,
                vertical=True,
                child=[
                    widgets.Label(
                        css_classes=["title-4", "accent"], label=player.bind("title")
                    ),
                    widgets.Label(label=player.bind("artist")),
                    widgets.Label(label=player.bind("album")),
                    widgets.Scale(
                        max=player.bind("length"),
                        value=player.bind("position"),
                        on_change=lambda self: asyncio.create_task(
                            player.set_position_async(self.value)
                        ),
                    ),
                    widgets.Label(label=track_progress(player)),
                    widgets.Box(
                        spacing=WIDGET_SPACING,
                        visible=player.bind("can_control"),
                        homogeneous=True,
                        child=[
                            mpris_button(
                                player,
                                "media-skip-backward",
                                lambda: asyncio.create_task(player.previous_async()),
                                "can_go_previous",
                            ),
                            mpris_button(
                                player,
                                player.bind(
                                    "playback_status",
                                    transform=lambda playback_status: "media-playback-paused"
                                    if playback_status == "Playing"
                                    else "media-playback-playing",
                                ),
                                lambda: asyncio.create_task(player.play_pause_async()),
                                "can_pause",
                            ),
                            mpris_button(
                                player,
                                "media-playback-stopped",
                                lambda: asyncio.create_task(player.stop_async()),
                            ),
                            mpris_button(
                                player,
                                "media-skip-forward",
                                lambda: asyncio.create_task(player.next_async()),
                                "can_go_next",
                            ),
                        ],
                    ),
                ],
            ),
        ],
    )


# TODO: give each player its own window
def media_player(monitor_id: int) -> widgets.Window:
    return widgets.Window(
        visible=False,
        namespace=f"ignis_popup_media_info_{monitor_id}",
        monitor=monitor_id,
        exclusivity="normal",
        anchor=["top"],
        margin_top=wm.GAP_WIDTH,
        child=widgets.Box(
            setup=lambda self: mpris.connect(
                "player-added", lambda _, player: self.append(mpris_player(player))
            ),
        ),
    )


def media(monitor_id: int) -> widgets.Box:
    media_player_window = media_player(monitor_id)
    return widgets.Box(
        spacing=WIDGET_SPACING,
        setup=lambda self: mpris.connect(
            "player-added",
            lambda _, player: self.append(mpris_title(player, media_player_window)),
        ),
    )


def desymbolize(icon_name: str | None) -> str:
    """Given an icon name, removes "-symbolic" from the end"""
    if icon_name is None:
        return ""
    else:
        return "-".join(icon_name.split("-")[:-1])


def volume() -> widgets.EventBox:
    slider_revealer = widgets.Revealer(
        child=widgets.Scale(
            step=5,
            value=audio.speaker.bind_many(
                ["volume", "is_muted"],
                lambda volume, is_muted: 0 if is_muted else volume,
            ),
            on_change=lambda self: audio.speaker.set_volume(self.value),
        ),
        transition_type="slide_left",
        reveal_child=False,
    )
    return widgets.EventBox(
        child=[
            widgets.Icon(
                image=audio.speaker.bind("icon_name", transform=desymbolize),
            ),
            widgets.Label(
                label=audio.speaker.bind(
                    "volume", transform=lambda volume: f"{volume}%"
                )
            ),
            slider_revealer,
        ],
        on_hover=lambda _: slider_revealer.set_reveal_child(True),
        on_hover_lost=lambda _: slider_revealer.set_reveal_child(False),
        on_right_click=lambda _: asyncio.create_task(
            utils.exec_sh_async("pavucontrol")
        ),
    )


def format_bitrate(kbits: int) -> str:
    prefix = ""
    magnitude = 0

    # NOTE: log undefined for zero
    if kbits > 0:
        magnitude = math.floor(math.log(kbits, 1000))

        # Higher magnitudes are too high to ever come up in practice
        match magnitude:
            case 0:
                prefix = "K"
            case 1:
                prefix = "M"
            case 2:
                prefix = "G"
            case _:
                pass

    if magnitude == 0:
        value = kbits
    else:
        value = round(kbits / (1000 * magnitude))

    return f"󰓅 {value} {prefix}bit/s"


def connection_name(device: WifiDevice) -> widgets.Box:
    return widgets.Box(
        child=[widgets.Label(label=device.ap.bind("ssid"))],
        tooltip_text=device.ap.bind("max_bitrate", transform=format_bitrate),
    )


def connections() -> widgets.EventBox:
    ssid_revealer = widgets.Revealer(
        child=widgets.Box(
            spacing=WIDGET_SPACING,
            child=network.wifi.bind(
                "devices",
                transform=lambda devices: [connection_name(d) for d in devices],
            ),
        ),
        transition_type="slide_left",
        reveal_child=False,
    )
    return widgets.EventBox(
        child=[
            widgets.Icon(image=network.wifi.bind("icon_name", transform=desymbolize)),
            ssid_revealer,
            widgets.Icon(image=network.vpn.bind("icon_name", transform=desymbolize)),
        ],
        on_right_click=lambda _: [
            asyncio.create_task(c.toggle_connection()) for c in network.vpn.connections
        ],
        on_hover=lambda _: ssid_revealer.set_reveal_child(True),
        on_hover_lost=lambda _: ssid_revealer.set_reveal_child(False),
        on_click=lambda _: asyncio.create_task(
            utils.exec_sh_async(f"{TERMINAL} --class='com.terminal.nmtui' -e nmtui")
        ),
    )


def statistic(
    css_class: str,
    icon_name: str,
    data: Callable[[], float],
    unit: str = "%",
    urgent: int = 70,
    critical: int = 90,
) -> widgets.Box:
    poll = utils.Poll(2000, lambda _: data())

    return widgets.Box(
        spacing=ICON_SPACING,
        css_classes=poll.bind(
            "output",
            transform=lambda output: [
                "error"
                if output >= critical
                else "warning"
                if output >= urgent
                else css_class,
            ],
        ),
        child=[
            widgets.Icon(image=icon_name),
            widgets.Label(
                label=poll.bind(
                    "output",
                    transform=lambda output: "{:02d}{}".format(round(output), unit),
                )
            ),
        ],
    )


def statistics() -> widgets.Box:
    def disk_usage() -> float:
        total, usage, _ = shutil.disk_usage(wm.PERSIST_DIR)
        return usage / total * 100

    return widgets.Box(
        spacing=WIDGET_SPACING,
        child=[
            statistic(
                "memory",
                "utilities-system-monitor",
                lambda: fetch.mem_used / fetch.mem_total * 100,
            ),
            statistic(
                "cpu",
                "firmware-manager",
                psutil.cpu_percent,
            ),
            statistic(
                "disk",
                "disk-manager",
                disk_usage,
            ),
            statistic(
                "temperature",
                "thermal-monitor",
                lambda: fetch.cpu_temp,
                "°C",
                60,
                75,
            ),
        ],
    )


def tray_item(item: SystemTrayItem) -> widgets.Button:
    if item.menu:
        menu = item.menu.copy()
    else:
        menu = None

    return widgets.Button(
        child=widgets.Box(
            child=[
                widgets.Icon(image=item.bind("icon")),
                menu,
            ]
        ),
        setup=lambda self: item.connect("removed", lambda _: self.unparent()),
        tooltip_text=item.bind("tooltip"),
        on_click=lambda _: menu.popup() if menu else None,
        on_right_click=lambda _: menu.popup() if menu else None,
        css_classes=["flat"],
    )


def tray() -> widgets.Box:
    return widgets.Box(
        spacing=WIDGET_SPACING,
        setup=lambda self: system_tray.connect(
            "added", lambda _, item: self.append(tray_item(item))
        ),
    )


def calendar(monitor_id: int) -> widgets.Window:
    return widgets.Window(
        visible=False,
        namespace=f"ignis_popup_calendar_{monitor_id}",
        monitor=monitor_id,
        exclusivity="normal",
        anchor=["top", "right"],
        margin_top=wm.GAP_WIDTH,
        margin_right=wm.GAP_WIDTH,
        child=widgets.Calendar(),
    )


def clock(monitor_id: int) -> widgets.EventBox:
    clock_label = widgets.Label()
    military_time = Variable(value=False)

    def update_clock(*_args) -> None:
        clock_label.set_label(
            datetime.now().strftime("%T" if military_time.value else "%r")
        )

    # Update the clock once per second
    utils.Poll(1000, update_clock)

    # Instantly update the clock when it changes to military time
    military_time.connect("notify::value", update_clock)

    # Calendar window widget
    calendar_window = calendar(monitor_id)

    return widgets.EventBox(
        spacing=WIDGET_SPACING,
        child=[widgets.Icon(image="accessories-clock"), clock_label],
        on_click=toggle_window(calendar_window),
        on_right_click=toggle_variable(military_time),
        tooltip_text=datetime.now().strftime(" %a, %b %d, %Y"),
    )


def battery_item(device: UPowerDevice) -> widgets.Box:
    return widgets.Box(
        setup=lambda self: device.connect("removed", lambda _: self.unparent()),
        child=[
            widgets.Icon(icon_name=device.bind("icon_name", transform=desymbolize)),
            widgets.Label(
                label=device.bind("percent", lambda percent: f"{int(percent)}%"),
            ),
        ],
        tooltip_text=device.bind_many(
            ["time_remaining", "charging", "charged"],
            lambda time_remaining, charging, charged: "󱟢 Battery is fully charged"
            if charged
            else strftime(
                "󱧥 %H:%M:%S " + ("until fully charged" if charging else "remaining"),
                gmtime(time_remaining),
            ),
        ),
    )


def battery() -> widgets.Box:
    if upower.is_available:
        return widgets.Box(
            setup=lambda self: upower.connect(
                "battery-added", lambda _, device: self.append(battery_item(device))
            ),
        )
    else:
        return widgets.Box()


def right_arrow() -> widgets.Arrow:
    return widgets.Icon(image="go-next")


def left_arrow() -> widgets.Arrow:
    return widgets.Icon(image="go-previous")


def bar(monitor_id: int) -> widgets.Window | None:
    monitor_name = utils.get_monitor(monitor_id)
    if monitor_name:
        monitor_name = monitor_name.get_connector()
        return widgets.Window(
            namespace=f"ignis_bar_{monitor_id}",
            monitor=monitor_id,
            exclusivity="exclusive",
            anchor=["left", "top", "right"],
            # NOTE: no margin_bottom because windows already have spacing
            margin_top=wm.GAP_WIDTH,
            margin_left=wm.GAP_WIDTH,
            margin_right=wm.GAP_WIDTH,
            child=widgets.CenterBox(
                start_widget=widgets.Box(
                    css_classes=["left"],
                    child=[
                        workspaces(monitor_name),
                        right_arrow(),
                        active_window(monitor_name),
                        right_arrow(),
                    ],
                ),
                center_widget=widgets.Box(
                    visible=mpris.bind("players", lambda value: len(value) != 0),
                    child=[
                        left_arrow(),
                        media(monitor_id),
                        right_arrow(),
                    ],
                ),
                end_widget=widgets.Box(
                    css_classes=["right"],
                    child=[
                        left_arrow(),
                        volume(),
                        left_arrow(),
                        connections(),
                        left_arrow(),
                        statistics(),
                        left_arrow(),
                        tray(),
                        left_arrow(),
                        clock(monitor_id),
                        battery(),
                    ],
                ),
            ),
        )


for i in range(utils.get_n_monitors()):
    _ = bar(i)
