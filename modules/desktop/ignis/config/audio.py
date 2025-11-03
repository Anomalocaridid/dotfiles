import asyncio

from common import desymbolize  # pyright: ignore[reportImplicitRelativeImport]
from ignis import utils, widgets
from ignis.services.audio import AudioService

audio = AudioService.get_default()


class Volume(widgets.EventBox):
    def __init__(self):
        slider_revealer = widgets.Revealer(
            child=widgets.Scale(
                step=5,
                value=audio.speaker.bind("volume"),
                on_change=lambda self: audio.speaker.set_volume(self.value),
            ),
            transition_type="slide_left",
            reveal_child=False,
        )

        super().__init__(
            child=[
                widgets.Icon(
                    image=audio.speaker.bind("icon_name", transform=desymbolize),
                ),
                widgets.Label(
                    label=audio.speaker.bind(
                        "volume", transform=lambda volume: f"{volume}%"
                    )
                ),
                slider_revealer,
            ],
            on_hover=lambda _: slider_revealer.set_reveal_child(True),
            on_hover_lost=lambda _: slider_revealer.set_reveal_child(False),
            on_right_click=lambda _: asyncio.create_task(
                utils.exec_sh_async("pavucontrol")
            ),
        )
