pragma ComponentBehavior: Bound

import QtQuick
import "../../components"
import "../../config"
import "../../services"

Row {
    id: root

    property bool expanded: false

    function clock(seconds: int): string {
        const p = n => String(n).padStart(2, "0");
        return `${p(Math.floor(seconds / 60))}:${p(seconds % 60)}`;
    }

    spacing: 0

    Chip {
        visible: Activities.recording
        tint: Colors.red
        icon: "fiber_manual_record"
        pulse: true
        value: root.clock(Recorder.elapsed)
        label: Recorder.kind === "voice" ? "Voice" : "Screen"

        Action { icon: "delete"; onActivated: Recorder.discard() }
        Action { icon: "stop"; tint: Colors.red; onActivated: Recorder.stop() }
    }

    Chip {
        visible: Activities.pomodoro
        icon: "timer"
        tint: Pomodoro.status === "break" ? Colors.green : Pomodoro.status === "paused" ? Colors.textMuted : Colors.primary
        pulse: Pomodoro.overtime
        value: Pomodoro.display
        label: Pomodoro.label

        Action { visible: Pomodoro.status !== "break"; icon: Pomodoro.status === "paused" ? "play_arrow" : "pause"; onActivated: Pomodoro.toggle() }
        Action { icon: Pomodoro.status === "break" ? "center_focus_strong" : "local_cafe"; onActivated: Pomodoro.swap() }
        Action { icon: "stop"; onActivated: Pomodoro.end() }
    }

    Repeater {
        model: Timers.floating

        Chip {
            required property var modelData

            readonly property var item: Timers.items.find(t => t.uid === modelData?.uid) ?? modelData ?? ({ kind: "timer", duration: 0, elapsed: 0, since: 0, running: false, done: false })
            readonly property bool countdown: item.kind === "timer"

            tint: item.done ? Colors.red : Colors.primary
            icon: countdown ? "hourglass_empty" : "timer"
            pulse: item.running
            value: Timers.clock(countdown ? Timers.remaining(item) : Timers.elapsed(item), false)
            label: item.done ? "Done" : countdown ? "Timer" : "Stopwatch"

            Action { icon: item.running ? "pause" : "play_arrow"; visible: !item.done; onActivated: Timers.toggle(item.uid) }
            Action { icon: "picture_in_picture_alt"; onActivated: Timers.pin(item.uid, true) }
            Action { icon: "close"; onActivated: Timers.remove(item.uid) }
        }
    }

    Chip {
        visible: Activities.camera
        tint: Colors.green
        icon: "videocam"
        label: "Camera"
    }

    Chip {
        visible: Activities.micOnly
        tint: Colors.orange
        icon: "mic"
        label: "Mic"
    }

    component Chip: Item {
        id: chip

        property color tint: Colors.primary
        property string icon: ""
        property string label: ""
        property string value: ""
        property bool pulse: false

        default property alias actions: actionRow.data

        readonly property int index: {
            let i = 0;
            for (const c of root.children) {
                if (c === chip)
                    break;
                if (c.visible)
                    i++;
            }
            return i;
        }

        width: row.implicitWidth + 24
        height: root.height

        Rectangle {
            visible: chip.index > 0
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter
            width: 1
            height: 14
            color: Colors.border
        }

        Row {
            id: row

            anchors.centerIn: parent
            spacing: 8

            MaterialIcon {
                anchors.verticalCenter: parent.verticalCenter
                text: chip.icon
                size: 14
                color: chip.tint

                SequentialAnimation on opacity {
                    running: chip.pulse
                    loops: Animation.Infinite

                    NumberAnimation { to: 0.35; duration: 800; easing.type: Easing.InOutSine }
                    NumberAnimation { to: 1; duration: 800; easing.type: Easing.InOutSine }

                    onRunningChanged: if (!running)
                        opacity = 1
                }
            }

            Text {
                visible: chip.value.length > 0
                anchors.verticalCenter: parent.verticalCenter
                text: chip.value
                color: Colors.textBright
                font.pixelSize: 12
                font.family: Fonts.family
                font.weight: Font.Medium
                font.features: { "tnum": 1 }
            }

            Text {
                anchors.verticalCenter: parent.verticalCenter
                text: chip.label
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }

            Row {
                id: actionRow

                anchors.verticalCenter: parent.verticalCenter
                spacing: 4
                width: root.expanded ? implicitWidth : 0
                opacity: root.expanded ? 1 : 0
                clip: true

                Behavior on width {
                    NumberAnimation { duration: 260; easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1] }
                }

                Behavior on opacity {
                    NumberAnimation { duration: 160 }
                }
            }
        }
    }

    component Action: Rectangle {
        id: btn

        property string icon: ""
        property color tint: Colors.textDimmed

        signal activated

        width: 22
        height: 22
        radius: 11
        color: area.containsMouse ? Colors.surfaceActive : "transparent"

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        MaterialIcon {
            anchors.centerIn: parent
            text: btn.icon
            size: 14
            color: area.containsMouse ? Colors.textBright : btn.tint
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
