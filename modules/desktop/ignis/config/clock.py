from datetime import datetime
from typing import Any, Callable

import wm  # pyright: ignore[reportMissingImports] # Custom module with constants from window manager config
from common import WIDGET_SPACING  # pyright: ignore[reportImplicitRelativeImport]
from ignis import utils, widgets
from ignis.gobject import Binding
from ignis.variable import Variable
from ignis.window_manager import WindowManager

window_manager = WindowManager.get_default()


class Calendar(widgets.Window):
    def __init__(self, monitor_id: int):
        super().__init__(
            visible=False,
            namespace=f"ignis_popup_calendar_{monitor_id}",
            monitor=monitor_id,
            exclusivity="normal",
            anchor=["top", "right"],
            margin_top=wm.GAP_WIDTH,
            margin_right=wm.GAP_WIDTH,
            child=widgets.Calendar(),
        )


class Clock(widgets.EventBox):
    __time = Variable()
    __24_hour_format = Variable(value=False)

    def __update_time(self, *_args: Any):  # pyright: ignore[reportAny, reportExplicitAny]
        self.__time.value = datetime.now()

    def __format_time(self, format: Callable[[], str]) -> Binding:
        return self.__time.bind(
            "value", transform=lambda value: value.strftime(format())
        )

    def __toggle_format(self, _: Any):  # pyright: ignore[reportExplicitAny]
        self.__24_hour_format.value ^= True

    def __init__(self, monitor_id: int):
        # Update the clock once per second
        _ = utils.Poll(1000, self.__update_time)

        # Instantly update the clock when it changes to 24-hour format
        self.__24_hour_format.connect("notify::value", self.__update_time)

        # Calendar window widget
        calendar = Calendar(monitor_id)

        super().__init__(
            spacing=WIDGET_SPACING,
            child=[
                widgets.Icon(image="accessories-clock"),
                widgets.Label(
                    label=self.__format_time(
                        lambda: "%T" if self.__24_hour_format.value else "%r"
                    )
                ),
            ],
            # Calendar window widget
            on_click=lambda _: window_manager.toggle_window(calendar.namespace),
            on_right_click=self.__toggle_format,
            tooltip_text=self.__format_time(lambda: " %a, %b %d, %Y"),
        )
