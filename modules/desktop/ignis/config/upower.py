from time import gmtime, strftime

from common import desymbolize  # pyright: ignore[reportImplicitRelativeImport]
from ignis import widgets
from ignis.services.upower import UPowerDevice, UPowerService

upower = UPowerService.get_default()


class BatteryItem(widgets.Box):
    def __init__(self, device: UPowerDevice):
        super().__init__(
            setup=lambda self: device.connect("removed", lambda _: self.unparent()),
            child=[
                widgets.Icon(icon_name=device.bind("icon_name", transform=desymbolize)),
                widgets.Label(
                    label=device.bind("percent", lambda percent: f"{int(percent)}%"),
                ),
            ],
            tooltip_text=device.bind_many(
                ["time_remaining", "charging", "charged"], self.__format_time_remaining
            ),
        )

    def __format_time_remaining(
        self, time_remaining: int, charging: bool, charged: bool
    ) -> str:
        if charged:
            return "󱟢 Battery is fully charged"
        else:
            return strftime("󱧥 %H:%M:%S ", gmtime(time_remaining)) + (
                "until fully charged" if charging else "remaining"
            )


class Battery(widgets.Box):
    def __init__(self):
        if upower.is_available:
            child = [
                widgets.Box(
                    setup=lambda self: upower.connect(
                        "battery-added",
                        lambda _, device: self.append(BatteryItem(device)),
                    ),
                )
            ]
        else:
            child = []

        super().__init__(child=child)
