import asyncio
from collections.abc import Callable
from time import gmtime, strftime

import wm  # pyright: ignore[reportMissingImports] # Custom module with constants from window manager config
from common import (  # pyright: ignore[reportImplicitRelativeImport]
    WIDGET_SPACING,
    toggle_window,
)
from ignis import widgets
from ignis.services.mpris import MprisPlayer, MprisService

mpris = MprisService.get_default()


def __track_progress(player: MprisPlayer):
    return player.bind_many(
        ["position", "length"],
        transform=lambda position,
        length: f"{strftime('%M:%S', gmtime(position))} / {strftime('%M:%S', gmtime(length))}",
    )


def __mpris_title(
    player: MprisPlayer, media_player_window: widgets.Window
) -> widgets.EventBox:
    artist_revealer = widgets.Revealer(
        child=widgets.Box(
            spacing=WIDGET_SPACING,
            child=[
                widgets.Label(
                    label=player.bind("artist"),
                    ellipsize="end",
                    max_width_chars=20,
                ),
                widgets.Label(
                    # Only show if there is an artist
                    visible=player.bind(
                        "artist",
                        transform=lambda artist: artist != "",
                    ),
                    label="-",
                ),
            ],
        ),
        reveal_child=False,
        transition_type="slide_right",
    )

    return widgets.EventBox(
        spacing=WIDGET_SPACING,
        setup=lambda self: player.connect(
            "closed",
            lambda _: self.unparent(),  # remove widget when player is closed
        ),
        child=[
            widgets.Icon(image="audio-x-generic"),
            artist_revealer,
            widgets.Label(
                ellipsize="end",
                max_width_chars=20,
                label=player.bind(
                    "title",
                ),
            ),
        ],
        tooltip_text=__track_progress(player),
        on_hover=lambda _: artist_revealer.set_reveal_child(True),
        on_hover_lost=lambda _: artist_revealer.set_reveal_child(False),
        on_click=toggle_window(media_player_window),
    )


def __mpris_button(
    player: MprisPlayer,
    icon_name: str,
    on_click: Callable[[], asyncio.Task[None]],
    visible_when: str = "can_control",
) -> widgets.Button:
    return widgets.Button(
        css_classes=["circular"],
        visible=player.bind(visible_when),
        child=widgets.Icon(pixel_size=32, image=icon_name),
        on_click=lambda _: on_click(),
    )


def __mpris_player(player: MprisPlayer) -> widgets.Box:
    return widgets.Box(
        spacing=WIDGET_SPACING,
        setup=lambda self: player.connect(
            "closed",
            lambda _: self.unparent(),  # remove widget when player is closed
        ),
        css_classes=["player"],
        child=[
            widgets.Picture(
                image=player.bind(
                    "art_url",
                    transform=lambda art_url: "applications-multimedia"
                    if art_url is None
                    else art_url,
                ),
                width=100,
                height=100,
            ),
            widgets.Box(
                spacing=WIDGET_SPACING,
                vertical=True,
                child=[
                    widgets.Label(
                        css_classes=["title-4", "accent"], label=player.bind("title")
                    ),
                    widgets.Label(label=player.bind("artist")),
                    widgets.Label(label=player.bind("album")),
                    widgets.Scale(
                        max=player.bind("length"),
                        value=player.bind("position"),
                        on_change=lambda self: asyncio.create_task(
                            player.set_position_async(self.value)
                        ),
                    ),
                    widgets.Label(label=__track_progress(player)),
                    widgets.Box(
                        spacing=WIDGET_SPACING,
                        visible=player.bind("can_control"),
                        homogeneous=True,
                        child=[
                            __mpris_button(
                                player,
                                "media-skip-backward",
                                lambda: asyncio.create_task(player.previous_async()),
                                "can_go_previous",
                            ),
                            __mpris_button(
                                player,
                                player.bind(
                                    "playback_status",
                                    transform=lambda playback_status: "media-playback-paused"
                                    if playback_status == "Playing"
                                    else "media-playback-playing",
                                ),
                                lambda: asyncio.create_task(player.play_pause_async()),
                                "can_pause",
                            ),
                            __mpris_button(
                                player,
                                "media-playback-stopped",
                                lambda: asyncio.create_task(player.stop_async()),
                            ),
                            __mpris_button(
                                player,
                                "media-skip-forward",
                                lambda: asyncio.create_task(player.next_async()),
                                "can_go_next",
                            ),
                        ],
                    ),
                ],
            ),
        ],
    )


# TODO: give each player its own window
def __media_player(monitor_id: int) -> widgets.Window:
    return widgets.Window(
        visible=False,
        namespace=f"ignis_popup_media_info_{monitor_id}",
        monitor=monitor_id,
        exclusivity="normal",
        anchor=["top"],
        margin_top=wm.GAP_WIDTH,
        child=widgets.Box(
            setup=lambda self: mpris.connect(
                "player-added", lambda _, player: self.append(__mpris_player(player))
            ),
        ),
    )


def media(monitor_id: int) -> widgets.Box:
    media_player_window = __media_player(monitor_id)
    return widgets.Box(
        spacing=WIDGET_SPACING,
        setup=lambda self: mpris.connect(
            "player-added",
            lambda _, player: self.append(__mpris_title(player, media_player_window)),
        ),
    )
