pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

ColumnLayout {
    id: root

    readonly property date first: {
        const d = new Date(Calendar.cursor.getFullYear(), Calendar.cursor.getMonth(), 1);
        d.setDate(1 - (d.getDay() + 6) % 7);
        return d;
    }

    property string editingId: ""
    property string dragId: ""
    property int dropIndex: -1

    spacing: 4

    Rectangle {
        visible: Calendar.view !== "month"
        Layout.fillWidth: true
        Layout.bottomMargin: 14
        implicitHeight: mini.implicitHeight + 20
        radius: 12
        color: Colors.surface

        ColumnLayout {
            id: mini

            anchors.fill: parent
            anchors.margins: 10
            spacing: 2

            Text {
                Layout.leftMargin: 4
                Layout.bottomMargin: 4
                text: Qt.formatDate(Calendar.cursor, "MMMM yyyy")
                color: Colors.textBright
                font.pixelSize: 12
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            Grid {
                Layout.fillWidth: true
                columns: 7

                Repeater {
                    model: ["M", "T", "W", "T", "F", "S", "S"]

                    Text {
                        required property string modelData

                        width: mini.width / 7
                        height: 20
                        text: modelData
                        color: Colors.textMuted
                        font.pixelSize: 9
                        font.family: Fonts.family
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                    }
                }

                Repeater {
                    model: 42

                    Item {
                        id: day

                        required property int index

                        readonly property date date: new Date(root.first.getFullYear(), root.first.getMonth(), root.first.getDate() + index)
                        readonly property string key: Qt.formatDate(date, "yyyy-MM-dd")
                        readonly property bool today: key === Clock.key
                        readonly property bool selected: key === Qt.formatDate(Calendar.selected, "yyyy-MM-dd")
                        readonly property bool inMonth: date.getMonth() === Calendar.cursor.getMonth()

                        width: mini.width / 7
                        height: 26

                        Rectangle {
                            anchors.centerIn: parent
                            width: 24
                            height: 24
                            radius: 12
                            color: day.today ? Colors.primary : day.selected ? Colors.surfaceActive : dayArea.containsMouse ? Colors.background : "transparent"

                            Text {
                                anchors.centerIn: parent
                                text: day.date.getDate()
                                color: day.today ? Colors.primaryText : day.inMonth ? Colors.text : Colors.textMuted
                                font.pixelSize: 10
                                font.family: Fonts.family
                                font.weight: day.today ? Font.Medium : Font.Normal
                            }

                            Rectangle {
                                visible: !day.today && Calendar.hasEvents(day.date)
                                anchors.horizontalCenter: parent.horizontalCenter
                                anchors.bottom: parent.bottom
                                anchors.bottomMargin: 2
                                width: 3
                                height: 3
                                radius: 1.5
                                color: Colors.textMuted
                            }
                        }

                        MouseArea {
                            id: dayArea
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: Calendar.goto(day.date)
                        }
                    }
                }
            }
        }
    }

    Text {
        Layout.leftMargin: 8
        Layout.bottomMargin: 4
        text: "Calendars"
        color: Colors.textMuted
        font.pixelSize: 11
        font.family: Fonts.family
        font.weight: Font.Medium
    }

    Flickable {
        Layout.fillWidth: true
        Layout.fillHeight: true
        contentHeight: list.implicitHeight
        boundsBehavior: Flickable.StopAtBounds
        clip: true

        ColumnLayout {
            id: list

            width: parent.width
            spacing: 2

            Repeater {
                model: Calendar.sources

                ColumnLayout {
                    id: src

                    required property var modelData

                    readonly property bool shown: !Calendar.hidden.includes(modelData.id)
                    readonly property bool editing: root.editingId === modelData.id

                    Layout.fillWidth: true
                    Layout.fillHeight: false
                    spacing: 0
                    opacity: root.dragId === modelData.id ? 0.4 : 1

                    Rectangle {
                        Layout.fillWidth: true
                        implicitHeight: 32
                        radius: 8
                        color: src.editing || rowHover.hovered ? Colors.surface : "transparent"

                        HoverHandler {
                            id: rowHover
                        }

                        MouseArea {
                            property real pressY: 0

                            anchors.fill: parent
                            cursorShape: root.dragId === src.modelData.id ? Qt.ClosedHandCursor : Qt.PointingHandCursor
                            preventStealing: true
                            onPressed: mouse => pressY = mouse.y
                            onPositionChanged: mouse => {
                                if (root.dragId === "" && Math.abs(mouse.y - pressY) > 4) {
                                    root.editingId = "";
                                    root.dragId = src.modelData.id;
                                }
                                if (root.dragId !== "") {
                                    const y = mapToItem(list, mouse.x, mouse.y).y;
                                    root.dropIndex = Math.max(0, Math.min(Calendar.sources.length, Math.round(y / 34)));
                                }
                            }
                            onReleased: {
                                if (root.dragId !== "")
                                    Calendar.moveSource(root.dragId, root.dropIndex);
                                root.dragId = "";
                                root.dropIndex = -1;
                            }
                            onCanceled: {
                                root.dragId = "";
                                root.dropIndex = -1;
                            }
                            onClicked: if (root.dragId === "")
                                Calendar.toggleSource(src.modelData.id)
                        }

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: 8
                            anchors.rightMargin: 4
                            spacing: 10

                            Rectangle {
                                implicitWidth: 16
                                implicitHeight: 16
                                radius: 4
                                color: src.shown ? src.modelData.color : "transparent"
                                border.width: 1.5
                                border.color: src.modelData.color

                                MaterialIcon {
                                    anchors.centerIn: parent
                                    visible: src.shown
                                    text: "done"
                                    size: 12
                                    color: Colors.background
                                }
                            }

                            Text {
                                Layout.fillWidth: true
                                text: src.modelData.name
                                color: src.shown ? Colors.text : Colors.textMuted
                                font.pixelSize: 12
                                font.family: Fonts.family
                                elide: Text.ElideRight
                            }

                            Nav {
                                visible: rowHover.hovered || src.editing
                                icon: "more_horiz"
                                onActivated: root.editingId = src.editing ? "" : src.modelData.id
                            }
                        }
                    }

                    Rectangle {
                        visible: src.editing
                        Layout.fillWidth: true
                        Layout.topMargin: 2
                        Layout.bottomMargin: 6
                        implicitHeight: editor.implicitHeight + 20
                        radius: 8
                        color: Colors.surface

                        ColumnLayout {
                            id: editor

                            anchors.fill: parent
                            anchors.margins: 10
                            spacing: 10

                            Field {
                                id: nameField

                                Layout.fillWidth: true
                                placeholder: "Name"
                                color: Colors.background
                                text: src.modelData.name
                                onAccepted: {
                                    Calendar.rename(src.modelData.id, text);
                                    root.editingId = "";
                                }
                                onEscaped: root.editingId = ""
                            }

                            ColorPicker {
                                Layout.fillWidth: true
                                colors: Calendar.palette
                                current: src.modelData.color
                                onPicked: color => Calendar.setColor(src.modelData.id, color)
                            }

                            RowLayout {
                                Layout.fillWidth: true
                                Layout.fillHeight: false
                                spacing: 6

                                Nav {
                                    visible: src.modelData.id !== "local"
                                    icon: "delete"
                                    onActivated: {
                                        root.editingId = "";
                                        Agenda.remove(src.modelData.id);
                                    }
                                }

                                Item { Layout.fillWidth: true }

                                Nav {
                                    icon: "done"
                                    onActivated: {
                                        Calendar.rename(src.modelData.id, nameField.text);
                                        root.editingId = "";
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Field {
                Layout.fillWidth: true
                Layout.topMargin: 8
                icon: "add"
                placeholder: "Subscribe to .ics link"
                onAccepted: {
                    Agenda.add(text);
                    text = "";
                }
            }
        }

        Rectangle {
            visible: root.dropIndex >= 0
            x: 6
            y: root.dropIndex * 34 - 2
            width: list.width - 12
            height: 2
            radius: 1
            color: Colors.primary
        }
    }

    Text {
        Layout.fillWidth: true
        Layout.leftMargin: 8
        Layout.topMargin: 8
        text: "t today · ←/→ move · tab view · n new · / search"
        color: Colors.textMuted
        font.pixelSize: 10
        font.family: Fonts.family
        wrapMode: Text.Wrap
    }

    component Nav: Rectangle {
        id: nav

        property string icon: ""

        signal activated

        implicitWidth: 24
        implicitHeight: 24
        radius: 12
        color: navArea.containsMouse ? Colors.surfaceActive : "transparent"

        MaterialIcon {
            anchors.centerIn: parent
            text: nav.icon
            size: 15
            color: navArea.containsMouse ? Colors.textBright : Colors.textMuted
        }

        MouseArea {
            id: navArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: nav.activated()
        }
    }
}
