from common import WIDGET_SPACING  # pyright: ignore[reportImplicitRelativeImport]
from ignis import widgets
from ignis.services.system_tray import SystemTrayItem, SystemTrayService

system_tray = SystemTrayService.get_default()


def tray_item(item: SystemTrayItem) -> widgets.Button:
    if item.menu:
        menu = item.menu.copy()
    else:
        menu = None

    return widgets.Button(
        child=widgets.Box(
            child=[
                widgets.Icon(image=item.bind("icon")),
                menu,
            ]
        ),
        setup=lambda self: item.connect("removed", lambda _: self.unparent()),
        tooltip_text=item.bind("tooltip"),
        on_click=lambda _: menu.popup() if menu else None,
        on_right_click=lambda _: menu.popup() if menu else None,
        css_classes=["flat"],
    )


def tray() -> widgets.Box:
    return widgets.Box(
        spacing=WIDGET_SPACING,
        setup=lambda self: system_tray.connect(
            "added", lambda _, item: self.append(tray_item(item))
        ),
    )
