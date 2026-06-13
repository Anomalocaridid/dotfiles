import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.WindowManager

RowLayout {
    Repeater {
        model: WindowManager.windowsets

        RowLayout {
            required property var modelData

            Label {
                font.pixelSize: Global.fontSize
                text: parent.modelData.name
            }
        }
    }
}
