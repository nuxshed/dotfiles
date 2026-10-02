import QtQuick
import Quickshell
import Quickshell.Widgets
import "../../components"
import "../../config"

Item {
    id: root

    property var info: null
    property int size: 18

    implicitWidth: size
    implicitHeight: size

    IconImage {
        anchors.fill: parent
        visible: !(root.info?.shell ?? false)
        source: root.info && !root.info.shell ? Quickshell.iconPath(root.info.icon, "application-x-executable") : ""
    }

    MaterialIcon {
        anchors.centerIn: parent
        visible: root.info?.shell ?? false
        text: "widgets"
        size: root.size - 2
        color: Colors.textMuted
    }
}
