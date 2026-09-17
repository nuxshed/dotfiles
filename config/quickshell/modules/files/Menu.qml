pragma ComponentBehavior: Bound

import QtQuick
import "../../components"
import "../../config"

Rectangle {
    id: root

    property var items: []
    property bool shown: false

    signal dismissed

    function popup(x: real, y: real, parentW: real, parentH: real): void {
        root.x = Math.max(6, Math.min(x, parentW - width - 6));
        root.y = Math.max(6, Math.min(y, parentH - height - 6));
        root.shown = true;
    }

    visible: opacity > 0
    opacity: root.shown ? 1 : 0
    scale: root.shown ? 1 : 0.96
    transformOrigin: Item.TopLeft
    width: 220
    height: column.implicitHeight + 12
    radius: 10
    color: Colors.background
    border.width: 1
    border.color: Colors.border
    z: 20

    Behavior on opacity {
        Anim { duration: 120 }
    }

    Behavior on scale {
        Anim { duration: 120 }
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.AllButtons
    }

    Column {
        id: column

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 6
        spacing: 1

        Repeater {
            model: root.items

            Item {
                id: row

                required property var modelData

                readonly property bool on: (modelData.enabled ?? true)

                width: column.width
                height: (modelData.divider ? 7 : 0) + 30

                Rectangle {
                    visible: row.modelData.divider ?? false
                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.leftMargin: 6
                    anchors.rightMargin: 6
                    y: 3
                    height: 1
                    color: Colors.border
                }

                Rectangle {
                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.bottom: parent.bottom
                    height: 30
                    radius: 6
                    color: mouse.containsMouse && row.on ? Colors.surfaceActive : "transparent"
                    opacity: row.on ? 1 : 0.4

                    MaterialIcon {
                        id: icon
                        anchors.left: parent.left
                        anchors.leftMargin: 8
                        anchors.verticalCenter: parent.verticalCenter
                        text: row.modelData.icon ?? ""
                        size: 16
                        color: row.modelData.danger ? Colors.red : Colors.textDimmed
                    }

                    Text {
                        anchors.left: parent.left
                        anchors.leftMargin: 32
                        anchors.right: shortcut.left
                        anchors.rightMargin: 8
                        anchors.verticalCenter: parent.verticalCenter
                        text: row.modelData.label
                        color: row.modelData.danger ? Colors.red : Colors.text
                        font.pixelSize: 12
                        font.family: Fonts.family
                        elide: Text.ElideRight
                    }

                    Text {
                        id: shortcut
                        anchors.right: parent.right
                        anchors.rightMargin: 10
                        anchors.verticalCenter: parent.verticalCenter
                        text: row.modelData.shortcut ?? ""
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }

                    MouseArea {
                        id: mouse
                        anchors.fill: parent
                        hoverEnabled: true
                        enabled: row.on
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            root.shown = false;
                            root.dismissed();
                            row.modelData.act();
                        }
                    }
                }
            }
        }
    }
}
