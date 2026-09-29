pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Item {
    id: root

    readonly property var info: Files.props
    readonly property bool shown: info !== null

    signal closed

    visible: opacity > 0
    opacity: root.shown ? 1 : 0
    z: 30

    Behavior on opacity {
        Anim { duration: 140 }
    }

    onShownChanged: {
        if (root.shown)
            keys.forceActiveFocus();
        else
            root.closed();
    }

    Item {
        id: keys
        Keys.onEscapePressed: Files.props = null
        Keys.onReturnPressed: Files.props = null
    }

    Rectangle {
        anchors.fill: parent
        color: "#000000"
        opacity: 0.4

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.AllButtons
            onClicked: Files.props = null
        }
    }

    Rectangle {
        anchors.centerIn: parent
        width: 420
        height: layout.implicitHeight + 40
        radius: 16
        color: Colors.background
        border.width: 1
        border.color: Colors.border
        scale: root.shown ? 1 : 0.96

        Behavior on scale {
            Anim { duration: 140 }
        }

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.AllButtons
        }

        ColumnLayout {
            id: layout

            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: parent.top
            anchors.margins: 20
            spacing: 14

            RowLayout {
                Layout.fillWidth: true
                spacing: 12

                Rectangle {
                    Layout.preferredWidth: 44
                    Layout.preferredHeight: 44
                    radius: 10
                    color: Colors.surface

                    MaterialIcon {
                        anchors.centerIn: parent
                        text: root.info ? Files.iconFor({ isDir: root.info.isDir, suffix: (root.info.name.lastIndexOf(".") > 0 ? root.info.name.slice(root.info.name.lastIndexOf(".") + 1) : "") }) : "insert_drive_file"
                        size: 24
                        color: root.info?.isDir ? Colors.primary : Colors.textDimmed
                    }
                }

                ColumnLayout {
                    Layout.fillWidth: true
                    spacing: 2

                    Text {
                        Layout.fillWidth: true
                        text: root.info?.name ?? ""
                        color: Colors.textBright
                        font.pixelSize: 14
                        font.family: Fonts.family
                        font.weight: Font.Medium
                        elide: Text.ElideMiddle
                    }

                    Text {
                        Layout.fillWidth: true
                        text: root.info ? Files.pretty(root.info.path) : ""
                        color: Colors.textMuted
                        font.pixelSize: 11
                        font.family: Fonts.family
                        elide: Text.ElideMiddle
                    }
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 1
                color: Colors.border
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 8

                Repeater {
                    model: {
                        const rows = root.info?.rows ?? [];
                        return rows.map(r => (r[0] === "Size" && root.info.isDir)
                            ? ["Size", (root.info.size.length > 0 ? root.info.size : "Calculating…") + (r[1].length > 0 ? " · " + r[1] : "")]
                            : r);
                    }

                    RowLayout {
                        required property var modelData

                        Layout.fillWidth: true
                        spacing: 12

                        Text {
                            Layout.preferredWidth: 90
                            Layout.alignment: Qt.AlignTop
                            text: modelData[0]
                            color: Colors.textMuted
                            font.pixelSize: 11
                            font.family: Fonts.family
                        }

                        Text {
                            Layout.fillWidth: true
                            text: modelData[1]
                            color: Colors.text
                            font.pixelSize: 11
                            font.family: Fonts.family
                            wrapMode: Text.WrapAnywhere
                        }
                    }
                }
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.topMargin: 4

                Item { Layout.fillWidth: true }

                TextButton {
                    text: "Copy path"
                    onClicked: {
                        Files.selected = [root.info.path];
                        Files.copyPath();
                    }
                }

                TextButton {
                    text: "Close"
                    primary: true
                    onClicked: Files.props = null
                }
            }
        }
    }
}
