from common import WIDGET_SPACING  # pyright: ignore[reportImplicitRelativeImport]
from ignis import widgets
from ignis.services.system_tray import SystemTrayItem, SystemTrayService

system_tray = SystemTrayService.get_default()


class TrayItem(widgets.Button):
    def __init__(self, item: SystemTrayItem):
        if item.menu:
            menu = item.menu.copy()
        else:
            menu = None

        def show_menu(_):
            if menu:
                menu.popup()
            else:
                None

        super().__init__(
            child=widgets.Box(
                child=[
                    widgets.Icon(image=item.bind("icon")),
                    menu,
                ]
            ),
            setup=lambda self: item.connect("removed", lambda _: self.unparent()),
            tooltip_text=item.bind("tooltip"),
            on_click=show_menu,
            on_right_click=show_menu,
            css_classes=["flat"],
        )


class Tray(widgets.Box):
    def __init__(self):
        super().__init__(
            spacing=WIDGET_SPACING,
            setup=lambda self: system_tray.connect(
                "added", lambda _, item: self.append(TrayItem(item))
            ),
        )
