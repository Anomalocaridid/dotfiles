import QtQuick.Controls
import QtQuick.Layouts
import Quickshell

Scope {
    Variants {
        model: Quickshell.screens

        PanelWindow {
            required property var modelData

            implicitHeight: 19
            screen: modelData

            anchors {
                left: true
                right: true
                top: true
            }

            Pane {
                anchors {
                    centerIn: parent
                    fill: parent
                }

                RowLayout {
                    anchors.verticalCenter: parent.verticalCenter
                    spacing: 0
                    width: parent.width

                    LeftBar {}

                    CenterBar {}

                    RightBar {}
                }
            }
        }
    }
}
