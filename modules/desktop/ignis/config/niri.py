from itertools import groupby

import wm  # pyright: ignore[reportmissingImports] # Custom module with constants from window manager config
from common import WIDGET_SPACING  # pyright: ignore[reportImplicitRelativeImport]
from ignis import utils, widgets
from ignis.services.applications import ApplicationsService
from ignis.services.niri import NiriService, NiriWindow, NiriWorkspace

niri = NiriService.get_default()
applications = ApplicationsService.get_default()


@utils.debounce(wm.SCROLL_COOLDOWN_MS)
def scroll_workspaces(monitor_name: str, step: int) -> None:
    current = [w for w in niri.workspaces if w.is_active and w.output == monitor_name][
        0
    ].idx
    niri.switch_to_workspace(min(max(current + step, 0), wm.WORKSPACES))


# Needed for files with icons with names different from their app icons/desktop files
def get_icon(app_id: str | None) -> str:
    # app_id can sometimes be unset
    if app_id is None:
        return "missing-image"

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


class WindowCount(widgets.Box):
    def __init__(self, app_id: str, is_focused: bool, count: int):
        # def __init__(self, workspace_idx: int, app_id: str, is_focused: bool, count: int):
        #     self.workspace_idx: int = workspace_idx
        super().__init__(
            child=[
                widgets.Icon(
                    image=get_icon(app_id),
                    css_classes=["focused"] if is_focused else [],
                ),
            ]
            + (
                [
                    # Show count in superscript
                    widgets.Label(
                        label="".join(
                            dict(zip("0123456789", "⁰¹²³⁴⁵⁶⁷⁸⁹")).get(digit, "")
                            for digit in str(count)
                        )
                    ),
                ]
                # Do not show count when there is only one
                if count > 1
                else []
            )
        )


class WorkspaceButton(widgets.Button):
    def __init__(
        self,
        workspace: NiriWorkspace,
        windows: list[NiriWindow],
    ):
        sorted_windows = sorted(
            # TODO: better handle floating windows (currently defaults to beginning of list)
            windows,
            key=lambda window: window.layout.pos_in_scrolling_layout or [0, 0],
        )

        window_counts = [
            WindowCount(app_id, is_focused, len(list(group)))
            for (app_id, is_focused), group in groupby(
                sorted_windows,
                key=lambda window: (
                    window.app_id,
                    window.is_focused,
                ),
            )
        ]

        super().__init__(
            css_classes=["flat"] + (["active"] if workspace.is_active else []),
            on_click=lambda _, id=workspace.idx: niri.switch_to_workspace(id),
            child=widgets.Box(
                child=[
                    widgets.Label(
                        label=f"{workspace.idx}{':' if window_counts else ''}"
                    )
                ]
                + window_counts
            ),
        )


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
                        ["workspaces", "windows"],
                        transform=lambda workspaces, windows: [
                            WorkspaceButton(
                                workspace,
                                [
                                    window
                                    for window in windows
                                    if window.workspace_id == workspace.id
                                ],
                            )
                            for workspace in workspaces
                        ],
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
