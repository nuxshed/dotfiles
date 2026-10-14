import QtQuick
import Quickshell
import "../../services"

FloatingWindow {
    id: root

    title: "Files"
    visible: Files.windowed
    implicitWidth: 980
    implicitHeight: 640
    minimumSize.width: 560
    minimumSize.height: 400
    color: "transparent"

    onVisibleChanged: if (!visible)
        Files.windowed = false

    FilesView {
        anchors.fill: parent
        windowed: true
        shown: root.visible
    }
}
