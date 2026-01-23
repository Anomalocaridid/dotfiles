import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets

RowLayout {
    spacing: 2

    IconImage {
        implicitSize: Global.iconSize
        source: Quickshell.iconPath("accessories-clock")
    }

    Label {
        font.pixelSize: Global.fontSize
        text: Qt.formatDateTime(clock.date, "hh:mm:ss AP")
    }

    SystemClock {
        id: clock

        precision: SystemClock.Seconds
    }
}
