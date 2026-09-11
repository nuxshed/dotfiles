import QtQuick
import "../config"

Row {
    id: root

    property int level: 0
    property color tint: Colors.textBright

    spacing: 2

    Repeater {
        model: 4

        Rectangle {
            required property int index

            anchors.bottom: parent.bottom
            width: 3
            height: 5 + index * 3
            radius: 1
            color: root.tint
            opacity: index < root.level ? 1 : 0.2

            Behavior on opacity {
                NumberAnimation { duration: 150 }
            }
        }
    }
}
