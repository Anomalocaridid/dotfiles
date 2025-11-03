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


class Statistic(widgets.Box):
    def __init__(
        self,
        css_class: str,
        icon_name: str,
        data: Callable[[], float],
        unit: str = "%",
        urgent: int = 70,
        critical: int = 90,
    ):
        poll = utils.Poll(2000, lambda _: data())

        super().__init__(
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


def disk_usage() -> float:
    total, usage, _ = shutil.disk_usage(wm.PERSIST_DIR)
    return usage / total * 100


class Statistics(widgets.Box):
    def __init__(self):
        super().__init__(
            spacing=WIDGET_SPACING,
            child=[
                Statistic(
                    "memory",
                    "utilities-system-monitor",
                    lambda: fetch.mem_used / fetch.mem_total * 100,
                ),
                Statistic(
                    "cpu",
                    "firmware-manager",
                    psutil.cpu_percent,
                ),
                Statistic(
                    "disk",
                    "disk-manager",
                    disk_usage,
                ),
                Statistic(
                    "temperature",
                    "thermal-monitor",
                    lambda: fetch.cpu_temp,
                    "°C",
                    60,
                    75,
                ),
            ],
        )
