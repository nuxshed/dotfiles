pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import "../../components"
import "../../config"
import "../../services"

PanelWindow {
    id: root

    readonly property int columns: 10
    readonly property int cell: 44
    readonly property var current: Emoji.results[grid.currentIndex] ?? null

    visible: Emoji.open
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:emoji"
    WlrLayershell.keyboardFocus: Emoji.open ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    onVisibleChanged: {
        if (visible) {
            input.text = "";
            input.forceActiveFocus();
        }
    }

    function move(delta: int): void {
        grid.currentIndex = Math.max(0, Math.min(grid.count - 1, grid.currentIndex + delta));
    }

    Rectangle {
        anchors.fill: parent
        color: "#000000"
        opacity: Emoji.open ? 0.45 : 0

        Behavior on opacity {
            NumberAnimation { duration: 150 }
        }

        MouseArea {
            anchors.fill: parent
            onClicked: Emoji.open = false
        }
    }

    Rectangle {
        id: card

        anchors.horizontalCenter: parent.horizontalCenter
        y: Math.round(parent.height * 0.22)
        width: root.columns * root.cell + 24
        height: layout.implicitHeight
        radius: 12
        color: Colors.background
        border.color: Colors.border
        border.width: 1
        clip: true
        opacity: Emoji.open ? 1 : 0
        scale: Emoji.open ? 1 : 0.97

        Behavior on opacity {
            Anim { duration: 160 }
        }

        Behavior on scale {
            Anim { duration: 160 }
        }

        MouseArea {
            anchors.fill: parent
        }

        ColumnLayout {
            id: layout

            width: parent.width
            spacing: 0

            RowLayout {
                Layout.fillWidth: true
                Layout.preferredHeight: 52
                Layout.leftMargin: 16
                Layout.rightMargin: 16
                spacing: 10

                MaterialIcon {
                    text: "search"
                    size: 20
                }

                TextInput {
                    id: input

                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    color: Colors.textBright
                    font.pixelSize: 14
                    font.family: Fonts.family
                    selectByMouse: true
                    selectionColor: Colors.primaryContainer
                    verticalAlignment: TextInput.AlignVCenter
                    onTextChanged: {
                        Emoji.query = text;
                        grid.currentIndex = 0;
                    }

                    Keys.onPressed: event => {
                        const shift = event.modifiers & (Qt.ShiftModifier | Qt.ControlModifier);
                        if (event.key === Qt.Key_Escape)
                            Emoji.open = false;
                        else if (event.key === Qt.Key_T && event.modifiers & Qt.ControlModifier)
                            Emoji.cycleTone();
                        else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter)
                            Emoji.pick(root.current, shift);
                        else if (event.key === Qt.Key_Tab)
                            Emoji.cycle(1);
                        else if (event.key === Qt.Key_Backtab)
                            Emoji.cycle(-1);
                        else if (event.key === Qt.Key_Down)
                            root.move(root.columns);
                        else if (event.key === Qt.Key_Up)
                            root.move(-root.columns);
                        else if (event.key === Qt.Key_Right && (input.text === "" || input.cursorPosition === input.text.length))
                            root.move(1);
                        else if (event.key === Qt.Key_Left && (input.text === "" || input.cursorPosition === 0))
                            root.move(-1);
                        else
                            return;
                        event.accepted = true;
                    }

                    Text {
                        anchors.fill: parent
                        visible: input.text.length === 0
                        text: "Search emoji…"
                        color: Colors.textMuted
                        font: input.font
                        verticalAlignment: Text.AlignVCenter
                    }
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 1
                color: Colors.border
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.preferredHeight: 44
                Layout.leftMargin: 12
                Layout.rightMargin: 12
                spacing: 0

                Repeater {
                    model: Emoji.categories

                    Rectangle {
                        id: tab

                        required property var modelData
                        required property int index

                        readonly property bool active: Emoji.query === "" && Emoji.category === index

                        Layout.fillWidth: true
                        Layout.preferredHeight: 32
                        radius: 8
                        color: active ? Colors.surfaceActive : tabHover.hovered ? Colors.surface : "transparent"

                        Behavior on color {
                            ColorAnimation { duration: 120 }
                        }

                        MaterialIcon {
                            anchors.centerIn: parent
                            text: tab.modelData.icon
                            size: 17
                            color: tab.active ? Colors.textBright : Colors.textMuted
                        }

                        HoverHandler {
                            id: tabHover
                            cursorShape: Qt.PointingHandCursor
                        }

                        MouseArea {
                            anchors.fill: parent
                            onClicked: {
                                input.text = "";
                                Emoji.category = tab.index;
                                grid.currentIndex = 0;
                            }
                        }
                    }
                }
            }

            GridView {
                id: grid

                Layout.preferredWidth: root.columns * root.cell
                Layout.preferredHeight: root.cell * 6
                Layout.alignment: Qt.AlignHCenter
                cellWidth: root.cell
                cellHeight: root.cell
                clip: true
                model: Emoji.results
                boundsBehavior: Flickable.StopAtBounds
                highlightMoveDuration: 0
                onCurrentIndexChanged: positionViewAtIndex(currentIndex, GridView.Contain)

                delegate: Item {
                    id: slot

                    required property var modelData
                    required property int index

                    width: root.cell
                    height: root.cell

                    Rectangle {
                        anchors.fill: parent
                        anchors.margins: 2
                        radius: 8
                        color: slot.index === grid.currentIndex ? Colors.surfaceActive : cellHover.hovered ? Colors.surface : "transparent"
                    }

                    Text {
                        anchors.centerIn: parent
                        text: Emoji.glyph(slot.modelData)
                        font.family: "Noto Color Emoji"
                        font.pixelSize: 24
                    }

                    HoverHandler {
                        id: cellHover
                        cursorShape: Qt.PointingHandCursor
                    }

                    MouseArea {
                        anchors.fill: parent
                        acceptedButtons: Qt.LeftButton | Qt.RightButton
                        onClicked: event => Emoji.pick(slot.modelData, event.button === Qt.RightButton)
                    }
                }

                Text {
                    anchors.centerIn: parent
                    visible: grid.count === 0
                    text: Emoji.query ? "No emoji found" : "Nothing used yet"
                    color: Colors.textMuted
                    font.pixelSize: 12
                    font.family: Fonts.family
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 1
                color: Colors.border
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.preferredHeight: 36
                Layout.leftMargin: 16
                Layout.rightMargin: 8
                spacing: 12

                Text {
                    Layout.fillWidth: true
                    text: root.current ? root.current.name.charAt(0).toUpperCase() + root.current.name.slice(1) : ""
                    color: Colors.textDimmed
                    font.pixelSize: 11
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }

                Text {
                    text: "↵ type    ⇧↵ copy"
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                }

                Rectangle {
                    Layout.preferredWidth: 26
                    Layout.preferredHeight: 26
                    radius: 8
                    color: toneHover.hovered ? Colors.surface : "transparent"

                    Text {
                        anchors.centerIn: parent
                        text: Emoji.tones[Emoji.tone]
                        font.family: "Noto Color Emoji"
                        font.pixelSize: 15
                    }

                    HoverHandler {
                        id: toneHover
                        cursorShape: Qt.PointingHandCursor
                    }

                    MouseArea {
                        anchors.fill: parent
                        onClicked: Emoji.cycleTone()
                    }
                }
            }
        }
    }
}
