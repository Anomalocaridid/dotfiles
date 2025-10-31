from time import gmtime, strftime

from common import desymbolize  # pyright: ignore[reportImplicitRelativeImport]
from ignis import widgets
from ignis.services.upower import UPowerDevice, UPowerService

upower = UPowerService.get_default()


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
