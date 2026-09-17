import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Variants {
    model: Quickshell.screens

    PanelWindow {
        id: win

        required property var modelData

        readonly property bool isActive: Hyprland.focusedMonitor?.name === modelData.name

        function clock(seconds) {
            const p = n => String(n).padStart(2, "0")
            return `${p(Math.floor(seconds / 60))}:${p(seconds % 60)}`
        }

        screen: modelData
        visible: isActive && Recorder.active
        exclusiveZone: 0
        color: "transparent"

        implicitWidth: pill.width + 80
        implicitHeight: 52

        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.namespace: "qs:record-indicator"

        anchors.top: true

        mask: Region {
            item: pill
        }

        Rectangle {
            id: pill

            anchors.top: parent.top
            anchors.horizontalCenter: parent.horizontalCenter
            width: row.implicitWidth + 26
            height: 36
            radius: height / 2
            topLeftRadius: 0
            topRightRadius: 0
            color: Colors.backgroundDeep

            RoundCorner {
                anchors.right: parent.left
                anchors.top: parent.top
                size: 12
                color: Colors.backgroundDeep
                corner: RoundCorner.CornerEnum.TopRight
            }

            RoundCorner {
                anchors.left: parent.right
                anchors.top: parent.top
                size: 12
                color: Colors.backgroundDeep
                corner: RoundCorner.CornerEnum.TopLeft
            }

            RowLayout {
                id: row
                anchors.centerIn: parent
                spacing: 12

                Rectangle {
                    Layout.preferredWidth: 7
                    Layout.preferredHeight: 7
                    radius: 3.5
                    color: Colors.red

                    SequentialAnimation on opacity {
                        running: Recorder.active
                        loops: Animation.Infinite

                        NumberAnimation {
                            to: 0.3
                            duration: 700
                            easing.type: Easing.InOutQuad
                        }
                        NumberAnimation {
                            to: 1
                            duration: 700
                            easing.type: Easing.InOutQuad
                        }
                    }
                }

                Text {
                    Layout.preferredWidth: 36
                    text: win.clock(Recorder.elapsed)
                    color: Colors.textBright
                    font.pixelSize: 13
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }

                Canvas {
                    id: wave

                    visible: Recorder.kind === "voice"
                    Layout.preferredWidth: 88
                    Layout.preferredHeight: 18

                    onPaint: {
                        const ctx = getContext("2d")
                        ctx.clearRect(0, 0, width, height)

                        const levels = Recorder.levels
                        const gap = 2
                        const barWidth = Math.max(1, (width - gap * (levels.length - 1)) / levels.length)
                        const mid = height / 2

                        ctx.fillStyle = Colors.red

                        for (let i = 0; i < levels.length; i++) {
                            const h = Math.max(2, levels[i] * height)
                            const x = i * (barWidth + gap)
                            ctx.beginPath()
                            ctx.roundedRect(x, mid - h / 2, barWidth, h, barWidth / 2, barWidth / 2)
                            ctx.fill()
                        }
                    }

                    Connections {
                        target: Recorder

                        function onLevelsChanged() {
                            wave.requestPaint()
                        }
                    }
                }

                IndicatorButton {
                    icon: "delete"
                    onActivated: Recorder.discard()
                }

                IndicatorButton {
                    icon: "stop"
                    danger: true
                    onActivated: Recorder.stop()
                }
            }
        }

        component IndicatorButton: Rectangle {
            id: btn

            property string icon: ""
            property bool danger: false

            signal activated

            Layout.preferredWidth: 26
            Layout.preferredHeight: 26
            radius: 7
            color: danger ? Qt.alpha(Colors.red, area.containsMouse ? 0.45 : 0.3) : (area.containsMouse ? Colors.surfaceActive : Colors.surface)

            Behavior on color {
                ColorAnimation {
                    duration: 140
                }
            }

            MaterialIcon {
                anchors.centerIn: parent
                text: btn.icon
                size: 14
                color: btn.danger ? Colors.red : Colors.textMuted
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
}
