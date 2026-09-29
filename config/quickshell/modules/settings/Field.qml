import QtQuick
import "../../config"

Rectangle {
    id: root

    property string value: ""
    property string placeholder: ""

    signal accepted(string text)

    implicitWidth: 240
    implicitHeight: 32
    radius: 9
    color: Colors.surface
    border.width: 1
    border.color: input.activeFocus ? Colors.primary : Colors.subtle

    Behavior on border.color {
        ColorAnimation { duration: 120 }
    }

    TextInput {
        id: input

        anchors.fill: parent
        anchors.leftMargin: 11
        anchors.rightMargin: 11
        verticalAlignment: TextInput.AlignVCenter
        text: root.value
        color: Colors.textBright
        selectionColor: Colors.primaryContainer
        selectedTextColor: Colors.textBright
        font.pixelSize: 12
        font.family: Fonts.family
        clip: true
        selectByMouse: true

        onAccepted: {
            root.accepted(text);
            focus = false;
        }
        onActiveFocusChanged: if (!activeFocus && text !== root.value) root.accepted(text)

        Text {
            anchors.fill: parent
            verticalAlignment: Text.AlignVCenter
            text: root.placeholder
            color: Colors.textMuted
            font: input.font
            visible: input.text.length === 0
        }
    }
}
