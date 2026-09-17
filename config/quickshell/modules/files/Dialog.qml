import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Item {
    id: root

    readonly property var info: Files.dialog
    readonly property bool shown: info !== null

    signal closed

    visible: opacity > 0
    opacity: root.shown ? 1 : 0
    z: 30

    Behavior on opacity {
        Anim { duration: 140 }
    }

    onShownChanged: {
        if (root.shown) {
            field.text = root.info.value ?? "";
            if (root.info.field) {
                field.input.forceActiveFocus();
                const dot = field.text.lastIndexOf(".");
                field.input.select(0, root.info.kind === "rename" && dot > 0 ? dot : field.text.length);
            } else {
                keys.forceActiveFocus();
            }
        } else {
            root.closed();
        }
    }

    Rectangle {
        anchors.fill: parent
        color: "#000000"
        opacity: 0.4

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.AllButtons
            onClicked: Files.dialog = null
        }
    }

    Item {
        id: keys
        Keys.onEscapePressed: Files.dialog = null
        Keys.onReturnPressed: Files.confirmDialog(field.text)
        Keys.onEnterPressed: Files.confirmDialog(field.text)
    }

    Rectangle {
        anchors.centerIn: parent
        width: 380
        height: layout.implicitHeight + 44
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
            anchors.margins: 22
            spacing: 14

            Text {
                Layout.fillWidth: true
                text: root.info?.title ?? ""
                color: Colors.textBright
                font.pixelSize: 14
                font.family: Fonts.family
                font.weight: Font.Medium
                elide: Text.ElideMiddle
            }

            Text {
                Layout.fillWidth: true
                visible: (root.info?.body ?? "").length > 0
                text: root.info?.body ?? ""
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
                wrapMode: Text.WordWrap
            }

            Field {
                id: field

                Layout.fillWidth: true
                visible: root.info?.field ?? false
                placeholder: "Name"
                onAccepted: Files.confirmDialog(text)
                onEscaped: Files.dialog = null
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.topMargin: 4
                spacing: 8

                Item { Layout.fillWidth: true }

                TextButton {
                    text: "Cancel"
                    onClicked: Files.dialog = null
                }

                TextButton {
                    text: root.info?.action ?? "OK"
                    primary: true
                    danger: root.info?.danger ?? false
                    enabled: !(root.info?.field ?? false) || Files.validName(field.text)
                    onClicked: Files.confirmDialog(field.text)
                }
            }
        }
    }
}
