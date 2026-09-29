pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import QtQuick.Shapes
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    readonly property color tint: Pomodoro.status === "break" ? Colors.green : Pomodoro.status === "paused" ? Colors.textMuted : Colors.primary
    readonly property var today: {
        const d = new Date(Pomodoro.now);
        d.setHours(0, 0, 0, 0);
        const a = d.getTime();
        return Pomodoro.totals(Pomodoro.sessionsBetween(a, a + 86400000), a, a + 86400000);
    }

    radius: 12
    color: Colors.surface

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 14
        spacing: 10

        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: false
            spacing: 6

            Rectangle {
                visible: Pomodoro.active
                implicitWidth: 7
                implicitHeight: 7
                radius: 3.5
                color: root.tint
            }

            Text {
                text: Pomodoro.label
                color: Colors.textBright
                font.pixelSize: 13
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            Text {
                Layout.fillWidth: true
                text: Pomodoro.active ? "·  block " + Pomodoro.block : "·  " + Pomodoro.duration(root.today.focus) + " today"
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
                elide: Text.ElideRight
            }

            Text {
                visible: Pomodoro.active
                text: Pomodoro.clock(Pomodoro.sessionElapsed)
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
                font.features: { "tnum": 1 }
            }

            Small {
                icon: "history"
                onActivated: Pomodoro.appOpen = true
            }
        }

        ColumnLayout {
            visible: !Pomodoro.active
            Layout.fillWidth: true
            Layout.fillHeight: true
            spacing: 4

            Item { Layout.fillHeight: true }

            Target { label: "Focus"; kind: "focus" }
            Target { label: "Break"; kind: "short" }
            Target { label: "Long break"; kind: "long"; hint: "every " + Pomodoro.longEvery }

            Item { Layout.fillHeight: true }

            Pill {
                Layout.fillWidth: true
                icon: "play_arrow"
                label: "Start session"
                accent: true
                onActivated: Pomodoro.start()
            }
        }

        ColumnLayout {
            visible: Pomodoro.active
            Layout.fillWidth: true
            Layout.fillHeight: true
            spacing: 12

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: 0

                Small { icon: "remove"; onActivated: Pomodoro.extend(-60) }

                Item {
                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    Item {
                        id: ring

                        anchors.centerIn: parent
                        width: Math.min(parent.width, parent.height)
                        height: width

                        WheelHandler {
                            onWheel: event => Pomodoro.extend(event.angleDelta.y > 0 ? 60 : -60)
                        }

                        Shape {
                            anchors.fill: parent
                            preferredRendererType: Shape.CurveRenderer

                            ShapePath {
                                strokeWidth: 4
                                strokeColor: Colors.subtle
                                fillColor: "transparent"

                                PathAngleArc {
                                    centerX: ring.width / 2
                                    centerY: ring.height / 2
                                    radiusX: ring.width / 2 - 3
                                    radiusY: radiusX
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
                                    centerX: ring.width / 2
                                    centerY: ring.height / 2
                                    radiusX: ring.width / 2 - 3
                                    radiusY: radiusX
                                    startAngle: -90
                                    sweepAngle: 360 * Pomodoro.progress

                                    Behavior on sweepAngle {
                                        NumberAnimation { duration: 600; easing.type: Easing.OutCubic }
                                    }
                                }
                            }
                        }

                        Column {
                            anchors.centerIn: parent
                            spacing: 0

                            Text {
                                anchors.horizontalCenter: parent.horizontalCenter
                                text: Pomodoro.display
                                color: Pomodoro.overtime ? root.tint : Colors.textBright
                                font.pixelSize: 24
                                font.family: Fonts.family
                                font.weight: Font.Medium
                                font.features: { "tnum": 1 }
                            }

                            Text {
                                anchors.horizontalCenter: parent.horizontalCenter
                                text: Pomodoro.overtime ? "over " + Pomodoro.clock(Pomodoro.target) : "of " + Pomodoro.clock(Pomodoro.target)
                                color: Colors.textMuted
                                font.pixelSize: 10
                                font.family: Fonts.family
                            }
                        }
                    }
                }

                Small { icon: "add"; onActivated: Pomodoro.extend(60) }
            }

            SegmentBar {
                Layout.fillWidth: true
                segments: Pomodoro.session?.segments ?? []
                from: Pomodoro.session?.start ?? 0
                to: Pomodoro.now
                now: Pomodoro.now
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: false
                spacing: 8

                Pill {
                    icon: "stop"
                    onActivated: Pomodoro.end()
                }

                Pill {
                    visible: Pomodoro.status !== "break"
                    Layout.fillWidth: true
                    icon: Pomodoro.status === "paused" ? "play_arrow" : "pause"
                    label: Pomodoro.status === "paused" ? "Resume" : "Pause"
                    onActivated: Pomodoro.toggle()
                }

                Pill {
                    Layout.fillWidth: true
                    icon: Pomodoro.status === "break" ? "center_focus_strong" : "local_cafe"
                    label: Pomodoro.status === "break" ? "Focus" : Pomodoro.nextLong ? "Long break" : "Break"
                    accent: true
                    fill: Pomodoro.status === "break" ? Colors.primary : Colors.green
                    onActivated: Pomodoro.swap()
                }
            }
        }
    }

    component Target: RowLayout {
        id: target

        property string label: ""
        property string kind: ""
        property string hint: ""

        Layout.fillWidth: true
        Layout.fillHeight: false
        spacing: 4

        Text {
            text: target.label
            color: Colors.text
            font.pixelSize: 12
            font.family: Fonts.family
        }

        Text {
            Layout.fillWidth: true
            leftPadding: 4
            text: target.hint
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family
        }

        Small { icon: "remove"; onActivated: Pomodoro.setTarget(target.kind, Pomodoro.targets[target.kind] - 60) }

        Text {
            Layout.preferredWidth: 42
            horizontalAlignment: Text.AlignHCenter
            text: Math.round(Pomodoro.targets[target.kind] / 60) + " min"
            color: Colors.textBright
            font.pixelSize: 12
            font.family: Fonts.family
            font.weight: Font.Medium
        }

        Small { icon: "add"; onActivated: Pomodoro.setTarget(target.kind, Pomodoro.targets[target.kind] + 60) }
    }

    component Small: Rectangle {
        id: btn

        property string icon: ""

        signal activated

        Layout.preferredWidth: 26
        Layout.preferredHeight: 26
        radius: 13
        color: smallArea.containsMouse ? Colors.subtle : "transparent"

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        MaterialIcon {
            anchors.centerIn: parent
            text: btn.icon
            size: 15
            color: smallArea.containsMouse ? Colors.textBright : Colors.textMuted
        }

        MouseArea {
            id: smallArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: btn.activated()
        }
    }

    component Pill: Rectangle {
        id: pill

        property string icon: ""
        property string label: ""
        property bool accent: false
        property color fill: Colors.primary

        signal activated

        Layout.preferredWidth: 36
        Layout.preferredHeight: 34
        radius: 10
        color: pill.accent ? Qt.tint(pill.fill, area.containsMouse ? "#18ffffff" : "transparent") : area.containsMouse ? Colors.subtle : Colors.surfaceActive

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        Row {
            anchors.centerIn: parent
            spacing: 5

            MaterialIcon {
                anchors.verticalCenter: parent.verticalCenter
                text: pill.icon
                size: 16
                color: pill.accent ? Colors.background : Colors.textDimmed
            }

            Text {
                visible: pill.label.length > 0
                anchors.verticalCenter: parent.verticalCenter
                text: pill.label
                color: pill.accent ? Colors.background : Colors.text
                font.pixelSize: 12
                font.family: Fonts.family
                font.weight: Font.Medium
            }
        }

        MouseArea {
            id: area
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: pill.activated()
        }
    }
}
