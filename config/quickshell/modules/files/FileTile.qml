import QtQuick
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
    readonly property string thumb: Thumbs.version >= 0 ? Thumbs.lookup(root.entry) : ""

    signal clicked(int modifiers)
    signal doubleClicked
    signal rightClicked(real x, real y)
    signal middleClicked
    signal dragStarted(int modifiers)
    signal dragEnded

    radius: 10
    color: root.dropTarget ? Colors.subtle : root.selected ? Colors.surfaceActive : mouse.containsMouse ? Colors.surface : "transparent"
    border.width: (root.current && !root.selected) || root.dropTarget ? 1 : 0
    border.color: root.dropTarget ? Colors.blue : Colors.subtle

    Column {
        anchors.fill: parent
        anchors.margins: 8
        spacing: 6
        opacity: root.dimmed || root.cut ? 0.45 : 1

        Rectangle {
            width: parent.width
            height: 88
            radius: 10
            color: image.status === Image.Ready ? Colors.surface : "transparent"
            clip: true

            Image {
                id: image
                anchors.fill: parent
                anchors.margins: 2
                source: root.thumb.length > 0 ? "file://" + root.thumb : ""
                asynchronous: true
                cache: true
                fillMode: Image.PreserveAspectFit
                sourceSize.width: 256
                sourceSize.height: 256
                smooth: true
                visible: status === Image.Ready
            }

            MaterialIcon {
                anchors.centerIn: parent
                visible: image.status !== Image.Ready
                text: Files.iconFor(root.entry)
                size: root.entry.isDir ? 52 : 44
                color: root.entry.isDir ? Colors.blue : root.selected ? Colors.textBright : Colors.textDimmed
            }
        }

        Text {
            width: parent.width
            text: root.entry.name
            color: root.selected ? Colors.textBright : root.entry.hidden ? Colors.textMuted : Colors.text
            font.pixelSize: 11
            font.family: Fonts.family
            horizontalAlignment: Text.AlignHCenter
            wrapMode: Text.WrapAnywhere
            maximumLineCount: 2
            elide: Text.ElideMiddle
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
