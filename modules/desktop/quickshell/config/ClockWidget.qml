import QtQuick.Controls
import Quickshell
import Quickshell.Widgets
import QtQuick.Layouts

RowLayout {
    IconImage {
        source: Quickshell.iconPath("accessories-clock")
        implicitSize: Global.iconSize
    }

    Label {
        text: Time.time
        font.pixelSize: Global.fontSize
    }
}
