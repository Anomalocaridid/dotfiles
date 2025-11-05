from collections import Counter, defaultdict

import unicodeit  # Not included by default # pyright: ignore[reportMissingTypeStubs]
import wm  # pyright: ignore[reportMissingImports] # Custom module with constants from window manager config
from common import WIDGET_SPACING  # pyright: ignore[reportImplicitRelativeImport]
from ignis import utils, widgets
from ignis.services.niri import NiriService, NiriWindow, NiriWorkspace
from ignis.services.applications import ApplicationsService

niri = NiriService.get_default()
applications = ApplicationsService.get_default()


@utils.debounce(wm.SCROLL_COOLDOWN_MS)
def scroll_workspaces(monitor_name: str, step: int) -> None:
    current = list(
        filter(lambda w: w.is_active and w.output == monitor_name, niri.workspaces)
    )[0].idx
    niri.switch_to_workspace(min(max(current + step, 0), wm.WORKSPACES))


# Needed for files with icons with names different from their app icons/desktop files
def get_icon(app_id: str) -> str:
    icon_name = utils.get_app_icon_name(app_id)

    if icon_name is not None:
        return icon_name

    app_results = applications.search(applications.apps, app_id)

    # If there is a desktop file for the open application, return the icon name
    if app_results:
        return app_results[0].icon
    else:
        # Otherwise, fall back to window's app id
        return app_id


# TODO: Preserve ordering of windows
class WorkspaceButton(widgets.Button):
    def __init__(
        self,
        workspace: NiriWorkspace,
        window_counts: dict[tuple[str, bool], int],
    ):
        super().__init__(
            css_classes=["flat"] + (["active"] if workspace.is_active else []),
            on_click=lambda _, id=workspace.idx: niri.switch_to_workspace(id),
            child=widgets.Box(
                child=[
                    widgets.Label(
                        label=f"{workspace.idx}{':' if window_counts else ''}"
                    )
                ]
                + [
                    widgets.Box(
                        child=[
                            widgets.Icon(
                                image=get_icon(app_id),
                                css_classes=["focused"] if is_focused else [],
                            ),
                            # Show count in superscript
                            widgets.Label(label=unicodeit.replace(f"^{{{count}}}"))
                            if count > 1
                            else None,
                        ]
                    )
                    for (app_id, is_focused), count in window_counts.items()
                ]
            ),
        )


def window_to_workspace_idx(workspaces: list[NiriWorkspace], window: NiriWindow) -> int:
    return list(filter(lambda ws: ws.id == window.workspace_id, workspaces))[0].idx


def format_workspaces(
    workspaces: list[NiriWorkspace], windows: list[NiriWindow]
) -> list[widgets.Button]:
    windows_by_workspace = defaultdict(Counter)

    for window in windows:
        # Only keep relevant information so that using windows are not counted as unique
        windows_by_workspace[window_to_workspace_idx(workspaces, window)].update(
            [(window.app_id, window.is_focused)]
        )
    return [WorkspaceButton(ws, windows_by_workspace[ws.idx]) for ws in workspaces]


class Workspaces(widgets.Box):
    def __init__(self, monitor_name: str):
        # Make sure to gracefully handle niri not being available
        if niri.is_available:
            child = [
                widgets.EventBox(
                    on_scroll_up=lambda _: scroll_workspaces(monitor_name, 1),
                    on_scroll_down=lambda _: scroll_workspaces(monitor_name, -1),
                    spacing=WIDGET_SPACING,
                    # Bind to active_window also to ensure focused window is up to date
                    child=niri.bind_many(
                        ["workspaces", "windows"], transform=format_workspaces
                    ),
                )
            ]
        else:
            child = []

        super().__init__(child=child)


class ActiveWindow(widgets.Box):
    def __init__(self, monitor_name: str):
        title = niri.bind(
            "active_window",
            transform=lambda active_window: active_window.title,
        )

        super().__init__(
            spacing=WIDGET_SPACING,
            visible=niri.bind(
                "active_output", lambda active_output: active_output == monitor_name
            ),
            child=[
                widgets.Icon(
                    image=niri.bind(
                        "active_window",
                        transform=lambda active_window: get_icon(active_window.app_id),
                    )
                ),
                widgets.Label(
                    ellipsize="end",
                    max_width_chars=15,
                    label=title,
                    tooltip_text=title,
                ),
            ],
        )
