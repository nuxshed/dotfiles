import QtQuick
import "../../components"
import "../../config"
import "../../services"

Row {
    readonly property bool shown: Activities.camera || Activities.micOnly

    spacing: 4

    MaterialIcon {
        anchors.verticalCenter: parent.verticalCenter
        visible: Activities.camera
        text: "videocam"
        size: 13
        color: Colors.green
    }

    MaterialIcon {
        anchors.verticalCenter: parent.verticalCenter
        visible: Activities.micOnly
        text: "mic"
        size: 13
        color: Colors.orange
    }
}
