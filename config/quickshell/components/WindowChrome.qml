import QtQuick
import "../config"

Item {
    id: root

    property int titleHeight: 52
    property int radius: 16
    property int grip: 8

    anchors.fill: parent
    z: 100

    Rectangle {
        parent: root.parent
        anchors.fill: parent
        z: -1
        radius: root.radius
        color: Colors.background
        border.width: 1
        border.color: Colors.border

        MouseArea {
            x: root.grip
            y: root.grip
            width: parent.width - root.grip * 2
            height: root.titleHeight - root.grip
            onPressed: root.Window.window?.startSystemMove()
        }
    }

    Rectangle {
        anchors.fill: parent
        radius: root.radius
        color: "transparent"
        border.width: 1
        border.color: Colors.border
    }

    Grip { edges: Qt.LeftEdge; x: 0; y: root.grip; width: root.grip; height: parent.height - root.grip * 2; cursorShape: Qt.SizeHorCursor }
    Grip { edges: Qt.RightEdge; x: parent.width - root.grip; y: root.grip; width: root.grip; height: parent.height - root.grip * 2; cursorShape: Qt.SizeHorCursor }
    Grip { edges: Qt.TopEdge; x: root.grip; y: 0; width: parent.width - root.grip * 2; height: root.grip; cursorShape: Qt.SizeVerCursor }
    Grip { edges: Qt.BottomEdge; x: root.grip; y: parent.height - root.grip; width: parent.width - root.grip * 2; height: root.grip; cursorShape: Qt.SizeVerCursor }
    Grip { edges: Qt.LeftEdge | Qt.TopEdge; x: 0; y: 0; width: root.grip; height: root.grip; cursorShape: Qt.SizeFDiagCursor }
    Grip { edges: Qt.RightEdge | Qt.BottomEdge; x: parent.width - root.grip; y: parent.height - root.grip; width: root.grip; height: root.grip; cursorShape: Qt.SizeFDiagCursor }
    Grip { edges: Qt.RightEdge | Qt.TopEdge; x: parent.width - root.grip; y: 0; width: root.grip; height: root.grip; cursorShape: Qt.SizeBDiagCursor }
    Grip { edges: Qt.LeftEdge | Qt.BottomEdge; x: 0; y: parent.height - root.grip; width: root.grip; height: root.grip; cursorShape: Qt.SizeBDiagCursor }

    component Grip: MouseArea {
        property int edges: 0

        onPressed: root.Window.window?.startSystemResize(edges)
    }
}
