import asyncio
import math
import shutil
from collections import Counter, defaultdict
from collections.abc import Callable
from datetime import datetime
from time import gmtime, strftime

import psutil  # Not included by default
import unicodeit  # Not included by default
import wm  # Custom module with constants from window manager config
from ignis.app import IgnisApp
from ignis.services.applications import ApplicationsService
from ignis.services.audio import AudioService
from ignis.services.fetch import FetchService
from ignis.services.mpris import MprisPlayer, MprisService
from ignis.services.network import NetworkService, WifiDevice
from ignis.services.niri import NiriService, NiriWindow, NiriWorkspace
from ignis.services.system_tray import SystemTrayItem, SystemTrayService
from ignis.utils import Utils
from ignis.variable import Variable
from ignis.widgets import Widget

app = IgnisApp.get_default()
app.apply_css(f"{Utils.get_current_dir()}/style.scss", "user")

applications = ApplicationsService.get_default()
audio = AudioService.get_default()
fetch = FetchService.get_default()
mpris = MprisService.get_default()
network = NetworkService.get_default()
niri = NiriService.get_default()
system_tray = SystemTrayService.get_default()

# Global constants
BAR_SPACING = 10
WIDGET_SPACING = 5
ICON_SPACING = 2

TERMINAL = "handlr launch x-scheme-handler/terminal --"


def toggle_variable(var: Variable):
    def _inner(_) -> None:
        var.value = not var.value

    return _inner


def toggle_window(window: Widget.Window):
    def _inner(_) -> None:
        window.visible = not window.visible

    return _inner


@Utils.debounce(wm.SCROLL_COOLDOWN_MS)
def scroll_workspaces(monitor_name: str, step: int) -> None:
    current = list(
        filter(lambda w: w.is_active and w.output == monitor_name, niri.workspaces)
    )[0].idx
    niri.switch_to_workspace(min(max(current + step, 0), 10))


# TODO: Preserve ordering of windows after that information is exposed in Niri IPC
# see: https://github.com/YaLTeR/niri/pull/1265
def workspace_button(
    workspace: NiriWorkspace,
    window_counts: dict[tuple[str, bool], int],
) -> Widget.Button:
    widget = Widget.Button(
        css_classes=["flat"],
        on_click=lambda _, id=workspace.idx: niri.switch_to_workspace(id),
        child=Widget.Box(
            child=[Widget.Label(label=f"{workspace.idx}{':' if window_counts else ''}")]
            + [
                Widget.Box(
                    child=[
                        Widget.Icon(
                            image=Utils.get_app_icon_name(app_id),
                            css_classes=["focused"] if is_focused else [],
                        ),
                        # Show count in superscript
                        Widget.Label(label=unicodeit.replace(f"^{{{count}}}"))
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
) -> list[Widget.Button]:
    windows_by_workspace = defaultdict(Counter)

    for window in windows:
        # Only keep relevant information so that using windows are not counted as unique
        windows_by_workspace[window_to_workspace_idx(workspaces, window)].update(
            [(window.app_id, window.is_focused)]
        )
    return [workspace_button(ws, windows_by_workspace[ws.idx]) for ws in workspaces]


def workspaces(monitor_name: str) -> Widget.EventBox:
    return Widget.EventBox(
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


def active_window(monitor_name: str) -> Widget.Box:
    title = niri.bind(
        "active_window",
        transform=lambda active_window: active_window.title,
    )

    return Widget.Box(
        spacing=WIDGET_SPACING,
        visible=niri.bind(
            "active_output", lambda active_output: active_output == monitor_name
        ),
        child=[
            Widget.Icon(
                image=niri.bind(
                    "active_window",
                    transform=lambda active_window: Utils.get_app_icon_name(
                        active_window.app_id
                    ),
                )
            ),
            Widget.Label(
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
    player: MprisPlayer, media_player_window: Widget.Window
) -> Widget.EventBox:
    artist_revealer = Widget.Revealer(
        child=Widget.Box(
            spacing=WIDGET_SPACING,
            child=[
                Widget.Label(
                    label=player.bind("artist"),
                    ellipsize="end",
                    max_width_chars=20,
                ),
                Widget.Label(
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

    return Widget.EventBox(
        spacing=WIDGET_SPACING,
        setup=lambda self: player.connect(
            "closed",
            lambda _: self.unparent(),  # remove widget when player is closed
        ),
        child=[
            Widget.Icon(image="audio-x-generic"),
            artist_revealer,
            Widget.Label(
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


def mpris_player(player: MprisPlayer) -> Widget.Box:
    return Widget.Box(
        spacing=WIDGET_SPACING,
        setup=lambda self: player.connect(
            "closed",
            lambda _: self.unparent(),  # remove widget when player is closed
        ),
        css_classes=["player"],
        child=[
            Widget.Picture(
                image=player.bind(
                    "art_url",
                    transform=lambda art_url: "applications-multimedia"
                    if art_url is None
                    else art_url,
                ),
                width=100,
                height=100,
            ),
            Widget.Box(
                spacing=WIDGET_SPACING,
                vertical=True,
                child=[
                    Widget.Label(
                        css_classes=["title-4", "accent"], label=player.bind("title")
                    ),
                    Widget.Label(label=player.bind("artist")),
                    Widget.Label(label=player.bind("album")),
                    Widget.Scale(
                        max=player.bind("length"),
                        value=player.bind("position"),
                        on_change=lambda self: player.set_property(
                            "position", self.value
                        ),
                    ),
                    Widget.Label(label=track_progress(player)),
                    Widget.Box(
                        spacing=WIDGET_SPACING,
                        visible=player.bind("can_control"),
                        homogeneous=True,
                        child=[
                            mpris_button(
                                player,
                                "media-skip-backward",
                                player.previous,
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
                                player.play_pause,
                                "can_pause",
                            ),
                            mpris_button(
                                player,
                                "media-playback-stopped",
                                player.stop,
                            ),
                            mpris_button(
                                player,
                                "media-skip-forward",
                                player.next,
                                "can_go_next",
                            ),
                        ],
                    ),
                ],
            ),
        ],
    )


# TODO: give each player its own window
def media_player(monitor_id: int) -> Widget.Window:
    return Widget.Window(
        visible=False,
        namespace=f"ignis_popup_media_info_{monitor_id}",
        monitor=monitor_id,
        exclusivity="normal",
        anchor=["top"],
        margin_top=wm.GAP_WIDTH,
        child=Widget.Box(
            setup=lambda self: mpris.connect(
                "player-added", lambda _, player: self.append(mpris_player(player))
            ),
        ),
    )


def media(monitor_id: int) -> Widget.Box:
    media_player_window = media_player(monitor_id)
    return Widget.Box(
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


def volume() -> Widget.EventBox:
    slider_revealer = Widget.Revealer(
        child=Widget.Scale(
            value=audio.speaker.bind_many(
                ["volume", "is_muted"],
                lambda volume, is_muted: 0 if is_muted else volume,
            ),
            on_change=lambda self: audio.speaker.set_volume(self.value),
        ),
        transition_type="slide_left",
        reveal_child=False,
    )
    return Widget.EventBox(
        child=[
            Widget.Icon(
                image=audio.speaker.bind("icon_name", transform=desymbolize),
            ),
            Widget.Label(
                label=audio.speaker.bind(
                    "volume", transform=lambda volume: f"{volume}%"
                )
            ),
            slider_revealer,
        ],
        on_hover=lambda _: slider_revealer.set_reveal_child(True),
        on_hover_lost=lambda _: slider_revealer.set_reveal_child(False),
        on_right_click=lambda _: asyncio.create_task(
            Utils.exec_sh_async("pavucontrol")
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


def connection_name(device: WifiDevice) -> Widget.Box:
    return Widget.Box(
        child=[Widget.Label(label=device.ap.bind("ssid"))],
        tooltip_text=device.ap.bind("max_bitrate", transform=format_bitrate),
    )


def connections() -> Widget.EventBox:
    ssid_revealer = Widget.Revealer(
        child=Widget.Box(
            spacing=WIDGET_SPACING,
            child=network.wifi.bind(
                "devices",
                transform=lambda devices: [connection_name(d) for d in devices],
            ),
        ),
        transition_type="slide_left",
        reveal_child=False,
    )
    return Widget.EventBox(
        child=[
            Widget.Icon(image=network.wifi.bind("icon_name", transform=desymbolize)),
            ssid_revealer,
            Widget.Icon(image=network.vpn.bind("icon_name", transform=desymbolize)),
        ],
        on_right_click=lambda _: [
            asyncio.create_task(c.toggle_connection()) for c in network.vpn.connections
        ],
        on_hover=lambda _: ssid_revealer.set_reveal_child(True),
        on_hover_lost=lambda _: ssid_revealer.set_reveal_child(False),
        on_click=lambda _: asyncio.create_task(
            Utils.exec_sh_async(f"{TERMINAL} --class='com.terminal.nmtui' -e nmtui")
        ),
    )


def statistic(
    css_class: str,
    icon_name: str,
    data: Callable[[], float],
    unit: str = "%",
    urgent: int = 70,
    critical: int = 90,
) -> Widget.Box:
    poll = Utils.Poll(2000, lambda _: data())

    return Widget.Box(
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
            Widget.Icon(image=icon_name),
            Widget.Label(
                label=poll.bind(
                    "output",
                    transform=lambda output: "{:02d}{}".format(round(output), unit),
                )
            ),
        ],
    )


def statistics() -> Widget.Box:
    def disk_usage() -> float:
        total, usage, _ = shutil.disk_usage("/persist")
        return usage / total * 100

    return Widget.Box(
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


def tray_item(item: SystemTrayItem) -> Widget.Button:
    if item.menu:
        menu = item.menu.copy()
    else:
        menu = None

    return Widget.Button(
        child=Widget.Box(
            child=[
                Widget.Icon(image=item.bind("icon")),
                menu,
            ]
        ),
        setup=lambda self: item.connect("removed", lambda _: self.unparent()),
        tooltip_text=item.bind("tooltip"),
        on_click=lambda _: menu.popup() if menu else None,
        on_right_click=lambda _: menu.popup() if menu else None,
        css_classes=["flat"],
    )


def tray() -> Widget.Box:
    return Widget.Box(
        spacing=WIDGET_SPACING,
        setup=lambda self: system_tray.connect(
            "added", lambda _, item: self.append(tray_item(item))
        ),
    )


def calendar(monitor_id: int) -> Widget.Window:
    return Widget.Window(
        visible=False,
        namespace=f"ignis_popup_calendar_{monitor_id}",
        monitor=monitor_id,
        exclusivity="normal",
        anchor=["top", "right"],
        margin_top=wm.GAP_WIDTH,
        margin_right=wm.GAP_WIDTH,
        child=Widget.Calendar(),
    )


def clock(monitor_id: int) -> Widget.EventBox:
    clock_label = Widget.Label()
    military_time = Variable(value=False)

    def update_clock(*_args) -> None:
        clock_label.set_label(
            datetime.now().strftime("%T" if military_time.value else "%r")
        )

    # Update the clock once per second
    Utils.Poll(1000, update_clock)

    # Instantly update the clock when it changes to military time
    military_time.connect("notify::value", update_clock)

    # Calendar window widget
    calendar_window = calendar(monitor_id)

    return Widget.EventBox(
        spacing=WIDGET_SPACING,
        child=[Widget.Icon(image="accessories-clock"), clock_label],
        on_click=toggle_window(calendar_window),
        on_right_click=toggle_variable(military_time),
        tooltip_text=datetime.now().strftime(" %a, %b %d, %Y"),
    )


def right_arrow() -> Widget.Arrow:
    return Widget.Icon(image="go-next")


def left_arrow() -> Widget.Arrow:
    return Widget.Icon(image="go-previous")


def bar(monitor_id: int) -> Widget.Window:
    monitor_name = Utils.get_monitor(monitor_id).get_connector()
    Widget.Window(
        namespace=f"ignis_bar_{monitor_id}",
        monitor=monitor_id,
        exclusivity="exclusive",
        anchor=["left", "top", "right"],
        # NOTE: no margin_bottom because windows already have spacing
        margin_top=wm.GAP_WIDTH,
        margin_left=wm.GAP_WIDTH,
        margin_right=wm.GAP_WIDTH,
        child=Widget.CenterBox(
            start_widget=Widget.Box(
                css_classes=["left"],
                child=[
                    workspaces(monitor_name),
                    right_arrow(),
                    active_window(monitor_name),
                    right_arrow(),
                ],
            ),
            center_widget=Widget.Box(
                visible=mpris.bind("players", lambda value: len(value) != 0),
                child=[
                    left_arrow(),
                    media(monitor_id),
                    right_arrow(),
                ],
            ),
            end_widget=Widget.Box(
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
                ],
            ),
        ),
    )


def mpris_button(
    player: MprisPlayer,
    icon_name: str,
    on_click: Callable[[], None],
    visible_when: str = "can_control",
) -> Widget.Button:
    return Widget.Button(
        css_classes=["circular"],
        visible=player.bind(visible_when),
        child=Widget.Icon(pixel_size=32, image=icon_name),
        on_click=lambda _: on_click(),
    )


for i in range(Utils.get_n_monitors()):
    bar(i)
