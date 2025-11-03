import wm  # pyright: ignore[reportMissingImports] # Custom module with constants from window manager config
from audio import Volume  # pyright: ignore[reportImplicitRelativeImport]
from clock import Clock  # pyright: ignore[reportImplicitRelativeImport]
from fetch import Statistics  # pyright: ignore[reportImplicitRelativeImport]
from ignis import utils, widgets
from ignis.css_manager import CssInfoPath, CssManager
from ignis.services.mpris import MprisService
from mpris import Media  # pyright: ignore[reportImplicitRelativeImport]
from network import Connections  # pyright: ignore[reportImplicitRelativeImport]
from niri import (  # pyright: ignore[reportImplicitRelativeImport]
    ActiveWindow,
    Workspaces,
)
from tray import Tray  # pyright: ignore[reportImplicitRelativeImport]
from upower import Battery  # pyright: ignore[reportImplicitRelativeImport]

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


class RightArrow(widgets.Icon):
    def __init__(self):
        super().__init__(image="go-next")


class LeftArrow(widgets.Icon):
    def __init__(self):
        super().__init__(image="go-previous")


class Bar(widgets.Window):
    def __init__(self, monitor_id: int):
        monitor_name = utils.get_monitor(monitor_id).get_connector()

        super().__init__(
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
                        Workspaces(monitor_name),
                        RightArrow(),
                        ActiveWindow(monitor_name),
                        RightArrow(),
                    ],
                ),
                center_widget=widgets.Box(
                    visible=mpris.bind("players", lambda value: len(value) != 0),
                    child=[
                        LeftArrow(),
                        Media(monitor_id),
                        RightArrow(),
                    ],
                ),
                end_widget=widgets.Box(
                    css_classes=["right"],
                    child=[
                        LeftArrow(),
                        Volume(),
                        LeftArrow(),
                        Connections(),
                        LeftArrow(),
                        Statistics(),
                        LeftArrow(),
                        Tray(),
                        LeftArrow(),
                        Clock(monitor_id),
                        Battery(),
                    ],
                ),
            ),
        )


for i in range(utils.get_n_monitors()):
    _ = Bar(i)
