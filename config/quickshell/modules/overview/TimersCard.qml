pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    readonly property var presets: [5, 10, 15, 25, 45, 60]

    radius: 12
    color: Colors.background

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        anchors.topMargin: 10
        spacing: 10

        Text {
            text: "Timers"
            color: Colors.textBright
            font.pixelSize: 13
            font.family: Fonts.family
            font.weight: Font.Medium
        }

        Launcher {
            Layout.fillWidth: true
            icon: "timer"
            label: "Stopwatch"
            hint: "pins to the screen"
            onActivated: Timers.stopwatch()
        }

        Text {
            text: "Countdown"
            color: Colors.textMuted
            font.pixelSize: 11
            font.family: Fonts.family
        }

        GridLayout {
            Layout.fillWidth: true
            columns: 3
            rowSpacing: 6
            columnSpacing: 6

            Repeater {
                model: root.presets

                Preset {
                    required property int modelData

                    Layout.fillWidth: true
                    minutes: modelData
                }
            }
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: 6

            Rectangle {
                Layout.fillWidth: true
                height: 30
                radius: 8
                color: Colors.surface
                border.width: 1
                border.color: custom.activeFocus ? Colors.outline : "transparent"

                TextInput {
                    id: custom

                    anchors.fill: parent
                    anchors.leftMargin: 10
                    anchors.rightMargin: 10
                    verticalAlignment: TextInput.AlignVCenter
                    color: Colors.text
                    font.pixelSize: 11
                    font.family: Fonts.family
                    validator: RegularExpressionValidator { regularExpression: /^\d{0,3}(:\d{0,2})?$/ }
                    selectByMouse: true

                    function submit(): void {
                        const parts = text.split(":");
                        const minutes = (+parts[0] || 0) + (+parts[1] || 0) / 60;
                        if (minutes > 0)
                            Timers.timer(minutes);
                        text = "";
                    }

                    onAccepted: submit()

                    Text {
                        anchors.fill: parent
                        verticalAlignment: Text.AlignVCenter
                        visible: custom.text.length === 0
                        text: "mm or mm:ss"
                        color: Colors.textMuted
                        font: custom.font
                    }
                }
            }

            Rectangle {
                width: 30
                height: 30
                radius: 8
                color: goArea.containsMouse ? Colors.surfaceActive : Colors.surface

                MaterialIcon {
                    anchors.centerIn: parent
                    text: "play_arrow"
                    size: 16
                    color: Colors.textDimmed
                }

                MouseArea {
                    id: goArea
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: custom.submit()
                }
            }
        }

        Item { Layout.fillHeight: true }
    }

    component Launcher: Rectangle {
        id: launch

        property string icon: ""
        property string label: ""
        property string hint: ""

        signal activated

        height: 44
        radius: 10
        color: launchArea.containsMouse ? Colors.surfaceActive : Colors.surface

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 12
            anchors.rightMargin: 12
            spacing: 10

            MaterialIcon {
                text: launch.icon
                size: 18
                color: Colors.primary
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 0

                Text {
                    text: launch.label
                    color: Colors.textBright
                    font.pixelSize: 12
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }

                Text {
                    text: launch.hint
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                }
            }

            MaterialIcon {
                text: "add"
                size: 15
                color: Colors.textMuted
            }
        }

        MouseArea {
            id: launchArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: launch.activated()
        }
    }

    component Preset: Rectangle {
        id: preset

        property int minutes: 5

        height: 30
        radius: 8
        color: presetArea.containsMouse ? Colors.surfaceActive : Colors.surface

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        Text {
            anchors.centerIn: parent
            text: preset.minutes + " min"
            color: presetArea.containsMouse ? Colors.textBright : Colors.text
            font.pixelSize: 11
            font.family: Fonts.family
        }

        MouseArea {
            id: presetArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: Timers.timer(preset.minutes)
        }
    }
}
