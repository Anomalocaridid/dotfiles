from datetime import datetime
from time import gmtime, strftime

import wm  # pyright: ignore[reportMissingImports] # Custom module with constants from window manager config
from audio import volume  # pyright: ignore[reportImplicitRelativeImport]
from common import (  # pyright: ignore[reportImplicitRelativeImport]
    WIDGET_SPACING,
    desymbolize,
    toggle_window,
)
from fetch import statistics  # pyright: ignore[reportImplicitRelativeImport]
from ignis import utils, widgets
from ignis.css_manager import CssInfoPath, CssManager
from ignis.services.mpris import MprisService
from ignis.services.upower import UPowerDevice, UPowerService
from ignis.variable import Variable
from mpris import media  # pyright: ignore[reportImplicitRelativeImport]
from network import connections  # pyright: ignore[reportImplicitRelativeImport]
from niri import (  # pyright: ignore[reportImplicitRelativeImport]
    active_window,
    workspaces,
)
from tray import tray  # pyright: ignore[reportImplicitRelativeImport]

css_manager = CssManager.get_default()
css_manager.apply_css(
    CssInfoPath(
        name="main",
        path=f"{utils.get_current_dir()}/style.scss",
        compiler_function=lambda path: utils.sass_compile(path=path),
        priority="user",
    )
)

mpris = MprisService.get_default()
upower = UPowerService.get_default()


def toggle_variable(var: Variable):
    def _inner(_) -> None:
        var.value = not var.value

    return _inner


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
