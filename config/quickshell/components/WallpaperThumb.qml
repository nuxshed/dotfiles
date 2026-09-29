import QtQuick
import Quickshell.Widgets
import "../config"

ClippingRectangle {
    id: root

    property string path: ""

    implicitWidth: 184
    implicitHeight: 115
    radius: 12
    color: Colors.surfaceActive

    Image {
        anchors.fill: parent
        source: root.path.length > 0 ? "file://" + root.path : ""
        sourceSize: Qt.size(480, 300)
        fillMode: Image.PreserveAspectCrop
        asynchronous: true
        smooth: true
        opacity: status === Image.Ready ? 1 : 0

        Behavior on opacity {
            NumberAnimation { duration: 200 }
        }
    }
}
