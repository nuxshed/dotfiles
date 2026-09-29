import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    required property string name
    required property string icon
    required property string path
    property string special: ""

    readonly property bool active: root.special.length > 0 ? Files.special === root.special : (Files.cwd === root.path && Files.special.length === 0)
    readonly property bool dropTarget: drop.containsDrag && !Files.dragPaths.includes(root.path)

    signal rightClicked(real x, real y)
    signal activated

    Layout.fillWidth: true
    Layout.preferredHeight: 32
    implicitWidth: row.implicitWidth + 36
    radius: 8
    color: root.dropTarget ? Colors.subtle : root.active ? Colors.surfaceActive : mouse.containsMouse ? Colors.surface : "transparent"
    border.width: root.dropTarget ? 1 : 0
    border.color: Colors.primary

    RowLayout {
        id: row

        anchors.fill: parent
        anchors.leftMargin: 10
        anchors.rightMargin: 10
        spacing: 10

        MaterialIcon {
            text: root.icon
            size: 17
            color: root.active ? Colors.primary : Colors.textDimmed
        }

        Text {
            Layout.fillWidth: true
            text: root.name
            color: root.active ? Colors.textBright : Colors.text
            font.pixelSize: 12
            font.family: Fonts.family
            elide: Text.ElideRight
        }
    }

    DropArea {
        id: drop
        anchors.fill: parent
        enabled: root.path.length > 0
        onDropped: event => {
            if (root.dropTarget)
                Files.dropOn(root.path, Files.dragPaths, event.proposedAction === Qt.CopyAction);
        }
    }

    MouseArea {
        id: mouse
        anchors.fill: parent
        hoverEnabled: true
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        cursorShape: Qt.PointingHandCursor
        onClicked: mouse => {
            if (mouse.button === Qt.RightButton)
                root.rightClicked(mouse.x, mouse.y);
            else if (root.path.length > 0)
                Files.navigate(root.path);
            else
                root.activated();
        }
    }
}
