import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    required property var entry
    required property bool selected
    required property bool current
    property bool dimmed: false
    property bool cut: false
    property Item dragGhost: null

    readonly property bool dropTarget: root.entry.isDir && drop.containsDrag && !Files.dragPaths.includes(root.entry.path)
    readonly property bool hasLocation: (root.entry.location ?? "").length > 0

    signal clicked(int modifiers)
    signal doubleClicked
    signal rightClicked(real x, real y)
    signal middleClicked
    signal dragStarted(int modifiers)
    signal dragEnded

    implicitHeight: 34
    radius: 8
    color: root.dropTarget ? Colors.subtle : root.selected ? Colors.surfaceActive : mouse.containsMouse ? Colors.surface : "transparent"
    border.width: (root.current && !root.selected) || root.dropTarget ? 1 : 0
    border.color: root.dropTarget ? Colors.blue : Colors.subtle

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: 10
        anchors.rightMargin: 12
        spacing: 10
        opacity: root.dimmed || root.cut ? 0.45 : 1

        MaterialIcon {
            text: Files.iconFor(root.entry)
            size: 18
            color: root.entry.isDir ? Colors.blue : root.selected ? Colors.textBright : Colors.textDimmed
        }

        Text {
            Layout.fillWidth: true
            text: root.entry.name
            color: root.selected ? Colors.textBright : root.entry.hidden ? Colors.textMuted : Colors.text
            font.pixelSize: 12
            font.family: Fonts.family
            elide: Text.ElideMiddle
        }

        Text {
            visible: root.hasLocation
            Layout.preferredWidth: 220
            text: root.entry.location ?? ""
            color: Colors.textMuted
            font.pixelSize: 11
            font.family: Fonts.family
            elide: Text.ElideLeft
            horizontalAlignment: Text.AlignRight
        }

        Text {
            visible: !root.hasLocation
            Layout.preferredWidth: 120
            text: Files.formatDate(root.entry.modified)
            color: Colors.textMuted
            font.pixelSize: 11
            font.family: Fonts.family
        }

        Text {
            visible: !root.hasLocation
            Layout.preferredWidth: 64
            text: root.entry.isDir ? "" : Files.formatSize(root.entry.size)
            color: Colors.textMuted
            font.pixelSize: 11
            font.family: Fonts.family
            horizontalAlignment: Text.AlignRight
        }
    }

    DropArea {
        id: drop
        anchors.fill: parent
        enabled: root.entry.isDir
        onDropped: event => {
            if (root.dropTarget)
                Files.dropOn(root.entry.path, Files.dragPaths, event.proposedAction === Qt.CopyAction);
        }
    }

    MouseArea {
        id: mouse

        property bool dragging: false
        property bool didDrag: false
        property point pressPos: Qt.point(0, 0)

        anchors.fill: parent
        hoverEnabled: true
        acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton

        onPressed: mouse => {
            pressPos = Qt.point(mouse.x, mouse.y);
            dragging = false;
        }

        onPositionChanged: mouse => {
            if (!pressedButtons || !root.dragGhost || (mouse.buttons & Qt.LeftButton) === 0)
                return;
            const p = mapToItem(root.dragGhost.parent, mouse.x, mouse.y);
            if (!dragging) {
                if (Math.abs(mouse.x - pressPos.x) < 8 && Math.abs(mouse.y - pressPos.y) < 8)
                    return;
                dragging = true;
                root.dragStarted(mouse.modifiers);
            }
            root.dragGhost.x = p.x + 14;
            root.dragGhost.y = p.y + 14;
        }

        onReleased: {
            if (dragging) {
                dragging = false;
                didDrag = true;
                root.dragEnded();
            }
        }

        onCanceled: {
            if (dragging) {
                dragging = false;
                root.dragEnded();
            }
        }

        onClicked: mouse => {
            if (didDrag) {
                didDrag = false;
                return;
            }
            if (mouse.button === Qt.RightButton)
                root.rightClicked(mouse.x, mouse.y);
            else if (mouse.button === Qt.MiddleButton)
                root.middleClicked();
            else
                root.clicked(mouse.modifiers);
        }

        onDoubleClicked: mouse => {
            if (mouse.button === Qt.LeftButton)
                root.doubleClicked();
        }
    }
}
