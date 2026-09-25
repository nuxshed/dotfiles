import QtQuick
import QtQuick.Shapes
import "../../components"
import "../../config"
import "../../services"

FloatingCard {
    id: root

    required property var modelData

    readonly property var item: Timers.items.find(t => t.uid === modelData?.uid) ?? modelData ?? ({ kind: "timer", duration: 0, elapsed: 0, since: 0, running: false, done: false })
    readonly property bool countdown: item.kind === "timer"
    readonly property real value: countdown ? Timers.remaining(item) : Timers.elapsed(item)
    readonly property real progress: countdown ? (item.duration > 0 ? 1 - value / item.duration : 0) : (value % 60000) / 60000
    readonly property color tint: item.done ? Colors.red : Colors.primary

    cardWidth: 148
    cardHeight: 148
    resizable: false
    posX: 140 + (modelData.uid % 6) * 24
    posY: 140 + (modelData.uid % 6) * 24

    Item {
        id: ring

        anchors.centerIn: parent
        anchors.verticalCenterOffset: -8
        width: 104
        height: 104

        Shape {
            anchors.fill: parent
            preferredRendererType: Shape.CurveRenderer

            ShapePath {
                strokeWidth: 4
                strokeColor: Colors.surfaceActive
                fillColor: "transparent"

                PathAngleArc {
                    centerX: 52
                    centerY: 52
                    radiusX: 48
                    radiusY: 48
                    startAngle: -90
                    sweepAngle: 360
                }
            }

            ShapePath {
                strokeWidth: 4
                strokeColor: root.tint
                fillColor: "transparent"
                capStyle: ShapePath.RoundCap

                PathAngleArc {
                    centerX: 52
                    centerY: 52
                    radiusX: 48
                    radiusY: 48
                    startAngle: -90
                    sweepAngle: 360 * root.progress

                    Behavior on sweepAngle {
                        enabled: root.countdown
                        NumberAnimation { duration: 220; easing.type: Easing.Linear }
                    }
                }
            }
        }

        Column {
            anchors.centerIn: parent
            spacing: 0

            Text {
                anchors.horizontalCenter: parent.horizontalCenter
                text: Timers.clock(root.value, false)
                color: Colors.textBright
                font.pixelSize: 20
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            Text {
                anchors.horizontalCenter: parent.horizontalCenter
                text: root.item.done ? "done" : root.countdown ? "timer" : "stopwatch"
                color: root.item.done ? Colors.red : Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family

                SequentialAnimation on opacity {
                    running: root.item.done
                    loops: Animation.Infinite

                    NumberAnimation { to: 0.3; duration: 500 }
                    NumberAnimation { to: 1; duration: 500 }

                    onRunningChanged: if (!running)
                        opacity = 1
                }
            }
        }
    }

    Row {
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 8
        spacing: 2

        Small { icon: root.item.running ? "pause" : "play_arrow"; enabled: !root.item.done; onActivated: Timers.toggle(root.item.uid) }
        Small { icon: "refresh"; onActivated: Timers.reset(root.item.uid) }
        Small { icon: "remove"; tip: "to island"; onActivated: Timers.pin(root.item.uid, false) }
        Small { icon: "close"; onActivated: Timers.remove(root.item.uid) }
    }

    component Small: Rectangle {
        id: btn

        property string icon: ""
        property string tip: ""

        signal activated

        width: 26
        height: 26
        radius: 13
        color: area.containsMouse ? Colors.surfaceActive : "transparent"
        opacity: enabled ? 1 : 0.35

        MaterialIcon {
            anchors.centerIn: parent
            text: btn.icon
            size: 14
            color: area.containsMouse ? Colors.textBright : Colors.textMuted
        }

        MouseArea {
            id: area
            anchors.fill: parent
            enabled: btn.enabled
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: btn.activated()
        }
    }
}
