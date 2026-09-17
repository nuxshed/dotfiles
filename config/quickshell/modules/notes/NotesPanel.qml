pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell.Wayland
import "../../components"
import "../../config"
import "../../services"

FloatingCard {
    id: root

    visible: Notes.open
    cardWidth: 400
    cardHeight: 320
    posX: 160
    posY: 160

    WlrLayershell.namespace: "qs:notes"
    WlrLayershell.keyboardFocus: Notes.open ? WlrKeyboardFocus.OnDemand : WlrKeyboardFocus.None

    onVisibleChanged: if (visible) editor.forceActiveFocus()

    function handleKey(event): void {
        if (event.key === Qt.Key_Escape)
            Notes.open = false;
        else if (event.modifiers & Qt.ControlModifier && event.key === Qt.Key_N)
            Notes.add();
        else if (event.modifiers & Qt.ControlModifier && event.key === Qt.Key_Tab)
            Notes.cycle(event.modifiers & Qt.ShiftModifier ? -1 : 1);
        else
            return;
        event.accepted = true;
    }

    Connections {
        target: Notes

        function onBodyChanged(): void {
            if (editor.text !== Notes.body)
                editor.text = Notes.body;
        }
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        Item {
            Layout.fillWidth: true
            Layout.preferredHeight: 42

            MouseArea {
                anchors.fill: parent
                cursorShape: drag.active ? Qt.ClosedHandCursor : Qt.OpenHandCursor
                drag.target: root.card
                drag.minimumX: 0
                drag.maximumX: root.width - root.card.width
                drag.minimumY: 0
                drag.maximumY: root.height - root.card.height
                drag.threshold: 2
            }

            RowLayout {
                anchors.fill: parent
                anchors.margins: 8
                anchors.bottomMargin: 0
                spacing: 4

                ListView {
                    id: tabs

                    Layout.fillWidth: true
                    Layout.preferredHeight: 26
                    orientation: ListView.Horizontal
                    spacing: 4
                    clip: true
                    interactive: false

                    WheelHandler {
                        onWheel: event => tabs.contentX = Math.max(0, Math.min(tabs.contentWidth - tabs.width, tabs.contentX - event.angleDelta.y))
                    }
                    model: Notes.notes
                    currentIndex: Notes.notes.findIndex(n => n.id === Notes.current)
                    highlightFollowsCurrentItem: true

                    delegate: Rectangle {
                        id: chip

                        required property var modelData

                        readonly property bool active: chip.modelData.id === Notes.current

                        width: Math.min(120, label.implicitWidth + 20)
                        height: 26
                        radius: 13
                        color: chip.active ? Colors.surfaceActive : chipHover.hovered ? Colors.surface : "transparent"

                        Behavior on color {
                            ColorAnimation { duration: 140 }
                        }

                        Text {
                            id: label
                            anchors.centerIn: parent
                            width: parent.width - 20
                            text: Notes.title(chip.modelData)
                            color: chip.active ? Colors.textBright : Colors.textMuted
                            font.pixelSize: 11
                            font.family: Fonts.family
                            font.weight: chip.active ? Font.Medium : Font.Normal
                            elide: Text.ElideRight
                            horizontalAlignment: Text.AlignHCenter
                        }

                        HoverHandler {
                            id: chipHover
                            cursorShape: Qt.PointingHandCursor
                        }

                        TapHandler {
                            onTapped: Notes.select(chip.modelData.id)
                        }
                    }
            }

            IconButton { icon: "add"; onActivated: Notes.add() }
            IconButton { icon: "delete"; onActivated: Notes.remove(Notes.current) }
            IconButton { icon: "close"; onActivated: Notes.open = false }
            }
        }

        Flickable {
            id: flick

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.margins: 8
            contentWidth: width
            contentHeight: editor.implicitHeight
            clip: true
            boundsBehavior: Flickable.StopAtBounds

            function follow(r: rect): void {
                if (r.y < contentY)
                    contentY = r.y;
                else if (r.y + r.height > contentY + height)
                    contentY = r.y + r.height - height;
            }

            TextEdit {
                id: editor

                width: flick.width
                padding: 6
                wrapMode: TextEdit.Wrap
                color: Colors.text
                selectionColor: Colors.primaryContainer
                selectedTextColor: Colors.textBright
                font.pixelSize: 12
                font.family: Fonts.family
                selectByMouse: true
                persistentSelection: true

                onTextChanged: Notes.setBody(text)
                onCursorRectangleChanged: flick.follow(cursorRectangle)

                Keys.onPressed: event => root.handleKey(event)

                Text {
                    anchors.fill: parent
                    padding: parent.padding
                    visible: editor.text.length === 0
                    text: "scratch…"
                    color: Colors.textMuted
                    font: editor.font
                }
            }
        }
    }

    component IconButton: Rectangle {
        id: btn

        property string icon: ""

        signal activated

        Layout.preferredWidth: 26
        Layout.preferredHeight: 26
        radius: 6
        color: area.containsMouse ? Colors.surfaceActive : "transparent"

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        MaterialIcon {
            anchors.centerIn: parent
            text: btn.icon
            size: 15
            color: area.containsMouse ? Colors.textBright : Colors.textMuted
        }

        MouseArea {
            id: area
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: btn.activated()
        }
    }
}
