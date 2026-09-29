pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    radius: 12
    color: Colors.surface

    Connections {
        target: Overview

        function onTabChanged(): void {
            if (Overview.tab === "tasks")
                input.forceActiveFocus();
        }
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        anchors.topMargin: 10
        spacing: 8

        RowLayout {
            Layout.fillWidth: true

            Text {
                text: "Tasks"
                color: Colors.textBright
                font.pixelSize: 13
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            Text {
                Layout.fillWidth: true
                text: Tasks.open.length > 0 ? Tasks.open.length + " open" : ""
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }

            Text {
                visible: Tasks.done.length > 0
                text: "clear done"
                color: clearHover.hovered ? Colors.textBright : Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family

                HoverHandler {
                    id: clearHover
                    cursorShape: Qt.PointingHandCursor
                }

                TapHandler {
                    onTapped: Tasks.clearDone()
                }
            }
        }

        Rectangle {
            Layout.fillWidth: true
            height: 32
            radius: 8
            color: Colors.surfaceActive
            border.width: 1
            border.color: input.activeFocus ? Colors.outline : "transparent"

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: 10
                anchors.rightMargin: 10
                spacing: 8

                MaterialIcon {
                    text: "add"
                    size: 15
                    color: Colors.textMuted
                }

                TextInput {
                    id: input

                    Layout.fillWidth: true
                    color: Colors.text
                    font.pixelSize: 12
                    font.family: Fonts.family
                    selectByMouse: true
                    clip: true

                    onAccepted: {
                        Tasks.add(text);
                        text = "";
                    }

                    Keys.onPressed: event => {
                        if (event.key === Qt.Key_Escape && text.length > 0) {
                            text = "";
                            event.accepted = true;
                        }
                    }

                    Text {
                        anchors.fill: parent
                        verticalAlignment: Text.AlignVCenter
                        visible: input.text.length === 0
                        text: "Add a task…"
                        color: Colors.textMuted
                        font: input.font
                    }
                }
            }
        }

        ListView {
            id: list

            Layout.fillWidth: true
            Layout.fillHeight: true
            clip: true
            spacing: 2
            boundsBehavior: Flickable.StopAtBounds
            model: Tasks.open.concat(Tasks.done)

            delegate: Rectangle {
                id: row

                required property var modelData

                width: list.width
                height: 28
                radius: 7
                color: rowArea.containsMouse ? Colors.surfaceActive : "transparent"

                Behavior on color {
                    ColorAnimation { duration: 120 }
                }

                MouseArea {
                    id: rowArea
                    anchors.fill: parent
                    hoverEnabled: true
                    onClicked: Tasks.toggle(row.modelData.id)
                }

                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: 8
                    anchors.rightMargin: 4
                    spacing: 10

                    Rectangle {
                        width: 15
                        height: 15
                        radius: 7.5
                        color: row.modelData.done ? Colors.primary : "transparent"
                        border.width: 1.5
                        border.color: row.modelData.done ? Colors.primary : Colors.outline

                        Behavior on color {
                            ColorAnimation { duration: 140 }
                        }

                        MaterialIcon {
                            anchors.centerIn: parent
                            visible: row.modelData.done
                            text: "done"
                            size: 11
                            color: Colors.primaryText
                        }
                    }

                    Text {
                        Layout.fillWidth: true
                        text: row.modelData.text
                        color: row.modelData.done ? Colors.textMuted : Colors.text
                        font.pixelSize: 12
                        font.family: Fonts.family
                        font.strikeout: row.modelData.done
                        elide: Text.ElideRight
                    }

                    Rectangle {
                        width: 22
                        height: 22
                        radius: 11
                        color: delArea.containsMouse ? Colors.subtle : "transparent"
                        opacity: rowArea.containsMouse || delArea.containsMouse ? 1 : 0

                        Behavior on opacity {
                            NumberAnimation { duration: 120 }
                        }

                        MaterialIcon {
                            anchors.centerIn: parent
                            text: "close"
                            size: 13
                            color: Colors.textMuted
                        }

                        MouseArea {
                            id: delArea
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: Tasks.remove(row.modelData.id)
                        }
                    }
                }
            }

            Text {
                anchors.centerIn: parent
                visible: list.count === 0
                text: "nothing to do"
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }
        }
    }
}
