from datetime import datetime
from ignis import utils, widgets

import wm  # pyright: ignore[reportMissingImports] # Custom module with constants from window manager config
from common import (  # pyright: ignore[reportImplicitRelativeImport]
    WIDGET_SPACING,
    toggle_window,
)
from ignis.variable import Variable


def __toggle_variable(var: Variable):
    def _inner(_) -> None:
        var.value = not var.value

    return _inner


def __calendar(monitor_id: int) -> widgets.Window:
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
    calendar_window = __calendar(monitor_id)

    return widgets.EventBox(
        spacing=WIDGET_SPACING,
        child=[widgets.Icon(image="accessories-clock"), clock_label],
        on_click=toggle_window(calendar_window),
        on_right_click=__toggle_variable(military_time),
        tooltip_text=datetime.now().strftime(" %a, %b %d, %Y"),
    )
