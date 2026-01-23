import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.UPower
import Quickshell.Widgets

RowLayout {
    spacing: 2
    visible: UPower.displayDevice.ready

    IconImage {
        implicitSize: Global.iconSize
        source: Quickshell.iconPath(UPower.displayDevice.iconName)
    }

    Label {
        font.pixelSize: Global.fontSize
        text: Math.round(UPower.displayDevice.percentage * 100) + "%"
    }
}
