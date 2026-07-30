import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Widgets
import "../../config"

Scope {
    id: root

    property bool shouldShowOsd: false
    property real brightnessLevel: 0

    Process {
        id: brightnessProcess
        command: ["brightnessctl", "get"]
        running: true

        stdout: StdioCollector {
            onStreamFinished: {
                var currentBrightness = parseFloat(this.text.trim())
                if (currentBrightness !== root.brightnessLevel) {
                    root.brightnessLevel = currentBrightness
                    root.shouldShowOsd = true
                    hideTimer.restart()
                }
            }
        }
    }

    Process {
        id: maxBrightnessProcess
        command: ["brightnessctl", "max"]
        running: true

        stdout: StdioCollector {
            onStreamFinished: {
                root.maxBrightness = parseFloat(this.text.trim())
            }
        }
    }

    property real maxBrightness: 100

    Timer {
        id: brightnessTimer
        interval: 100
        running: true
        repeat: true
        onTriggered: brightnessProcess.running = true
    }

    Timer {
        id: hideTimer
        interval: 2000
        onTriggered: root.shouldShowOsd = false
    }

    LazyLoader {
        active: root.shouldShowOsd

        PanelWindow {
            anchors.bottom: true
            margins.bottom: screen.height / 13
            exclusiveZone: 0

            implicitWidth: 300
            implicitHeight: 60
            color: "transparent"

            mask: Region {}

            Rectangle {
                anchors.centerIn: parent
                width: parent.width - 40
                height: parent.height - 20
                radius: 12
                color: Colors.surface
                border.color: Colors.border
                border.width: 1

                RowLayout {
                    anchors.centerIn: parent
                    width: parent.width - 30
                    spacing: 15

                    Rectangle {
                        Layout.fillWidth: true
                        implicitHeight: 8
                        radius: 4
                        color: Colors.border

                        Rectangle {
                            anchors {
                                left: parent.left
                                top: parent.top
                                bottom: parent.bottom
                            }
                            width: parent.width * (root.brightnessLevel / root.maxBrightness)
                            radius: parent.radius
                            color: Colors.text
                        }
                    }

                    Text {
                        text: Math.round((root.brightnessLevel / root.maxBrightness) * 100) + "%"
                        color: Colors.text
                        font.pixelSize: 14
                        font.family: "Cartograph CF"
                        font.bold: true
                        Layout.preferredWidth: 40
                    }
                }
            }
        }
    }
}
