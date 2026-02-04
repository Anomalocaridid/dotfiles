pragma ComponentBehavior: Bound

import Niri
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets

RowLayout {
    Repeater {
        model: niri.workspaces

        Rectangle {
            id: workspace

            required property int index
            required property var model

            // TODO: take colors from theme

            color: model.isActive ? "#000000" : "#333333"
            height: parent.height
            implicitWidth: windows.implicitWidth
            radius: 10

            RowLayout {
                id: windows

                Repeater {
                    // TODO: Only make windows appear in their own workspace
                    model: niri.windows

                    Rectangle {
                        id: window

                        required property var model

                        // TODO: take colors from theme

                        color: model.isFocused ? "lightblue" : "white"
                        height: parent.height
                        implicitWidth: icon.implicitWidth
                        width: parent.width

                        // TODO: group contiguous runs of the same app

                        IconImage {
                            id: icon

                            implicitSize: Global.iconSize
                            source: Quickshell.iconPath(DesktopEntries.byId(parent.model.appId)?.icon ?? "image-missing")
                        }

                        MouseArea {
                            acceptedButtons: Qt.LeftButton | Qt.RightButton
                            anchors.fill: parent

                            onClicked: function (mouseEvent) {
                                if (mouseEvent.button === Qt.LeftButton) {
                                    niri.focusWindow(parent.model.id);
                                } else {
                                    niri.closeWindow(parent.model.id);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Niri {
        id: niri

        Component.onCompleted: connect()
        onConnected: console.log("Connected to niri")
        onErrorOccurred: function (error) {
            console.error("Error:", error);
        }
    }
}
