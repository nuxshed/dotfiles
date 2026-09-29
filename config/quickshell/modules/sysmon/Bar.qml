import QtQuick
import "../../config"

Rectangle {
    id: root

    property real value: 0
    property color accent: Colors.primary

    implicitHeight: 6
    radius: 3
    color: Colors.subtle

    Rectangle {
        width: Math.max(0, Math.min(1, root.value)) * parent.width
        height: parent.height
        radius: 3
        color: root.accent

        Behavior on width { NumberAnimation { duration: 400; easing.type: Easing.OutCubic } }
    }
}
