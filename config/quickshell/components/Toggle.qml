import QtQuick
import "../config"

Item {
    id: root

    property bool checked: false

    signal toggled()

    implicitWidth: 40
    implicitHeight: 22

    Rectangle {
        anchors.fill: parent
        radius: height / 2
        color: root.checked ? Colors.primary : Colors.subtle
        opacity: root.enabled ? 1 : 0.4

        Behavior on color {
            ColorAnimation { duration: 180 }
        }

        Rectangle {
            x: root.checked ? parent.width - width - 3 : 3
            anchors.verticalCenter: parent.verticalCenter
            width: root.checked ? 16 : 12
            height: width
            radius: width / 2
            color: root.checked ? Colors.primaryText : Colors.textMuted

            Behavior on x {
                NumberAnimation { duration: 180; easing.type: Easing.OutCubic }
            }

            Behavior on width {
                NumberAnimation { duration: 180; easing.type: Easing.OutCubic }
            }

            Behavior on color {
                ColorAnimation { duration: 180 }
            }
        }
    }

    HoverHandler {
        cursorShape: Qt.PointingHandCursor
    }

    TapHandler {
        onTapped: root.toggled()
    }
}
