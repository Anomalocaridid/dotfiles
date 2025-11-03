import asyncio
import math

from common import (  # pyright: ignore[reportImplicitRelativeImport]
    TERMINAL,
    WIDGET_SPACING,
    desymbolize,
)
from ignis import utils, widgets
from ignis.services.network import NetworkService, WifiDevice

network = NetworkService.get_default()


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


class ConnectionName(widgets.Box):
    def __init__(self, device: WifiDevice):
        super().__init__(
            child=[widgets.Label(label=device.ap.bind("ssid"))],
            tooltip_text=device.ap.bind("max_bitrate", transform=format_bitrate),
        )


class Connections(widgets.EventBox):
    def __init__(self):
        ssid_revealer = widgets.Revealer(
            child=widgets.Box(
                spacing=WIDGET_SPACING,
                child=network.wifi.bind(
                    "devices",
                    transform=lambda devices: [ConnectionName(d) for d in devices],
                ),
            ),
            transition_type="slide_left",
            reveal_child=False,
        )

        super().__init__(
            child=[
                widgets.Icon(
                    image=network.wifi.bind("icon_name", transform=desymbolize)
                ),
                ssid_revealer,
                widgets.Icon(
                    image=network.vpn.bind("icon_name", transform=desymbolize)
                ),
            ],
            on_right_click=lambda _: [
                asyncio.create_task(c.toggle_connection())
                for c in network.vpn.connections
            ],
            on_hover=lambda _: ssid_revealer.set_reveal_child(True),
            on_hover_lost=lambda _: ssid_revealer.set_reveal_child(False),
            on_click=lambda _: asyncio.create_task(
                utils.exec_sh_async(f"{TERMINAL} --class='com.terminal.nmtui' -e nmtui")
            ),
        )
