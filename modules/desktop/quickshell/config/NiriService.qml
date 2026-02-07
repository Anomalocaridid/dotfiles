pragma Singleton
import QtQuick

import Quickshell
import Quickshell.Io

Singleton {
    id: root

    Component.onCompleted: {
        socket.connected = true;
    }

    Socket {
        id: socket

        connected: false
        path: Quickshell.env("NIRI_SOCKET") || ""

        parser: SplitParser {
            onRead: data => {
                try {
                    const parsed = JSON.parse(data.trim());
                    root._handleEvent(parsed);
                } catch (e) {
                    console.error("NiriState: parse error", e);
                }
            }
        }

        onConnectedChanged: {
            if (connected) {
                write(JSON.stringify("EventStream") + "\n");
                flush();
            }
        }
    }
}
