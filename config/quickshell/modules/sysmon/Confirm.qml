import QtQuick
import QtQuick.Layouts
import "../../config"
import "../files"

Item {
    id: root

    property string title: ""
    property string body: ""
    property string action: ""
    property var onAccept: null
    property bool shown: false

    function ask(title: string, body: string, action: string, cb: var): void {
        root.title = title;
        root.body = body;
        root.action = action;
        root.onAccept = cb;
        root.shown = true;
    }

    visible: opacity > 0
    opacity: shown ? 1 : 0
    z: 30

    Behavior on opacity { NumberAnimation { duration: 120 } }

    Rectangle {
        anchors.fill: parent
        color: Qt.alpha(Colors.background, 0.6)
        radius: 10

        MouseArea {
            anchors.fill: parent
            onClicked: root.shown = false
        }
    }

    Rectangle {
        anchors.centerIn: parent
        width: 340
        height: col.implicitHeight + 40
        radius: 12
        color: Colors.background
        border.width: 1
        border.color: Colors.outline

        MouseArea { anchors.fill: parent }

        ColumnLayout {
            id: col
            anchors.fill: parent
            anchors.margins: 20
            spacing: 8

            Text {
                text: root.title
                color: Colors.textBright
                font.pixelSize: 13
                font.family: Fonts.family
                font.weight: Font.Medium
            }
            Text {
                Layout.fillWidth: true
                text: root.body
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
                wrapMode: Text.Wrap
            }
            RowLayout {
                Layout.topMargin: 8
                Layout.alignment: Qt.AlignRight
                spacing: 6

                TextButton {
                    text: "Cancel"
                    onClicked: root.shown = false
                }
                TextButton {
                    text: root.action
                    primary: true
                    danger: true
                    onClicked: {
                        root.shown = false;
                        if (root.onAccept)
                            root.onAccept();
                    }
                }
            }
        }
    }
}
