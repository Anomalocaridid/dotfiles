import shutil
from collections.abc import Callable
from ignis import utils, widgets
import wm  # pyright: ignore[reportMissingImports] # Custom module with constants from window manager config

import psutil  # Not included by default
from common import (  # pyright: ignore[reportImplicitRelativeImport]
    ICON_SPACING,
    WIDGET_SPACING,
)

from ignis.services.fetch import FetchService

fetch = FetchService.get_default()


def __statistic(
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
            __statistic(
                "memory",
                "utilities-system-monitor",
                lambda: fetch.mem_used / fetch.mem_total * 100,
            ),
            __statistic(
                "cpu",
                "firmware-manager",
                psutil.cpu_percent,
            ),
            __statistic(
                "disk",
                "disk-manager",
                disk_usage,
            ),
            __statistic(
                "temperature",
                "thermal-monitor",
                lambda: fetch.cpu_temp,
                "°C",
                60,
                75,
            ),
        ],
    )
