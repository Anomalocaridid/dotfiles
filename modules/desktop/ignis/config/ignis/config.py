import wm  # pyright: ignore[reportMissingImports] # Custom module with constants from window manager config
from audio import volume  # pyright: ignore[reportImplicitRelativeImport]
from fetch import statistics  # pyright: ignore[reportImplicitRelativeImport]
from ignis import utils, widgets
from ignis.css_manager import CssInfoPath, CssManager
from ignis.services.mpris import MprisService
from mpris import media  # pyright: ignore[reportImplicitRelativeImport]
from network import connections  # pyright: ignore[reportImplicitRelativeImport]
from niri import (  # pyright: ignore[reportImplicitRelativeImport]
    active_window,
    workspaces,
)
from tray import tray  # pyright: ignore[reportImplicitRelativeImport]
from clock import clock  # pyright: ignore[reportImplicitRelativeImport]
from upower import battery  # pyright: ignore[reportImplicitRelativeImport]

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
