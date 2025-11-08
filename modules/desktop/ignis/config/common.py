# Global constants
WIDGET_SPACING = 5
ICON_SPACING = 2

TERMINAL = "handlr launch x-scheme-handler/terminal --"


def desymbolize(icon_name: str | None) -> str:
    """Given an icon name, removes "-symbolic" from the end"""
    if icon_name is None:
        return ""
    else:
        return "-".join(icon_name.split("-")[:-1])
