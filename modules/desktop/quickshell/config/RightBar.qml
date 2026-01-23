import QtQuick.Layouts

RowLayout {
    Layout.alignment: Qt.AlignRight | Qt.AlignVCenter
    // FIXME: Qt.AlignVCenter does not work and this anchor is needed to align to the right
    anchors.verticalCenter: parent.verticalCenter

    ClockWidget {
    }

    BatteryWidget {
    }
}
