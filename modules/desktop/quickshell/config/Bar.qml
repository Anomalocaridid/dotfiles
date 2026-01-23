import Quickshell
import QtQuick.Controls
import QtQuick.Layouts

Scope {
    Variants {
        model: Quickshell.screens

        PanelWindow {
            required property var modelData
            screen: modelData

            implicitHeight: 19

            anchors {
                top: true
                left: true
                right: true
            }

            Pane {
                anchors {
                    centerIn: parent
                    fill: parent
                }

                RowLayout {
                    anchors {
                        left: parent.left
                        right: parent.right
                        verticalCenter: parent.verticalCenter
                    }

                    ClockWidget {
                        Layout.alignment: Qt.AlignRight
                    }
                }
            }
        }
    }
}
