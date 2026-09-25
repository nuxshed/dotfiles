import QtQuick
import "../../components"
import "../../config"

Rectangle {
    id: root

    property alias text: input.text
    property alias input: input
    property string placeholder: ""
    property string icon: ""
    property string label: ""

    signal accepted
    signal escaped

    implicitHeight: 32
    radius: 8
    color: Colors.surface
    border.width: 1
    border.color: input.activeFocus ? Colors.outline : "transparent"

    Behavior on border.color {
        ColorAnimation { duration: 150 }
    }

    MaterialIcon {
        visible: root.icon.length > 0
        anchors.left: parent.left
        anchors.leftMargin: 9
        anchors.verticalCenter: parent.verticalCenter
        text: root.icon
        size: 15
        color: Colors.textMuted
    }

    Text {
        id: labelText
        visible: root.label.length > 0
        anchors.left: parent.left
        anchors.leftMargin: 10
        anchors.verticalCenter: parent.verticalCenter
        text: root.label
        color: Colors.textMuted
        font.pixelSize: 11
        font.family: Fonts.family
    }

    TextInput {
        id: input

        anchors.fill: parent
        anchors.leftMargin: root.icon.length > 0 ? 30 : root.label.length > 0 ? labelText.implicitWidth + 18 : 10
        anchors.rightMargin: 10
        color: Colors.textBright
        font.pixelSize: 12
        font.family: Fonts.family
        selectByMouse: true
        selectionColor: Colors.primaryContainer
        selectedTextColor: Colors.textBright
        clip: true
        verticalAlignment: TextInput.AlignVCenter
        horizontalAlignment: root.label.length > 0 ? TextInput.AlignRight : TextInput.AlignLeft

        onAccepted: root.accepted()
        Keys.onEscapePressed: root.escaped()

        Text {
            anchors.fill: parent
            visible: input.text.length === 0
            text: root.placeholder
            color: Colors.textMuted
            font: input.font
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: input.horizontalAlignment
            elide: Text.ElideRight
        }
    }
}
