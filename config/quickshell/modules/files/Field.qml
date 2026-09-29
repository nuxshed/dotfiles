import QtQuick
import "../../components"
import "../../config"

Rectangle {
    id: root

    property alias text: input.text
    property alias input: input
    property string placeholder: ""
    property string icon: ""

    signal accepted
    signal escaped
    signal down

    implicitHeight: 34
    radius: 10
    color: Colors.surface
    border.width: 1
    border.color: input.activeFocus ? Colors.primary : Colors.border

    Behavior on border.color {
        ColorAnimation { duration: 150 }
    }

    MaterialIcon {
        visible: root.icon.length > 0
        anchors.left: parent.left
        anchors.leftMargin: 10
        anchors.verticalCenter: parent.verticalCenter
        text: root.icon
        size: 16
        color: Colors.textMuted
    }

    TextInput {
        id: input

        anchors.fill: parent
        anchors.leftMargin: root.icon.length > 0 ? 32 : 12
        anchors.rightMargin: 12
        color: Colors.textBright
        font.pixelSize: 12
        font.family: Fonts.family
        selectByMouse: true
        selectionColor: Colors.primaryContainer
        selectedTextColor: Colors.textBright
        clip: true
        verticalAlignment: TextInput.AlignVCenter

        onAccepted: root.accepted()
        Keys.onEscapePressed: root.escaped()
        Keys.onDownPressed: root.down()

        Text {
            anchors.fill: parent
            visible: input.text.length === 0
            text: root.placeholder
            color: Colors.textMuted
            font: input.font
            verticalAlignment: Text.AlignVCenter
            elide: Text.ElideRight
        }
    }
}
