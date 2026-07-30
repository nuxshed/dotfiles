import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Pipewire
import Quickshell.Widgets
import "../../config"

Scope {
    id: root

    PwObjectTracker {
        objects: [ Pipewire.defaultAudioSink ]
    }

    Connections {
        target: Pipewire.defaultAudioSink?.audio

        function onVolumeChanged() {
            root.shouldShowOsd = true;
            hideTimer.restart();
        }
    }

    property bool shouldShowOsd: false

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

            // An empty click mask prevents the window from blocking mouse events.
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

                    // Main volume bar (up to 100%)
                    Rectangle {
                        Layout.fillWidth: true
                        implicitHeight: 8
                        radius: 4
                        color: Colors.border

                        // Main bar (gray)
                        Rectangle {
                            anchors {
                                left: parent.left
                                top: parent.top
                                bottom: parent.bottom
                            }
                            width: parent.width * Math.min(Pipewire.defaultAudioSink?.audio.volume ?? 0, 1)
                            radius: parent.radius
                            color: Colors.text
                        }

                        // overflow
                        Rectangle {
                            anchors {
                                left: parent.left
                                top: parent.top
                                bottom: parent.bottom
                            }
                            y: -10
                            height: parent.height
                            width: parent.width * Math.max((Pipewire.defaultAudioSink?.audio.volume ?? 0) - 1, 0)
                            radius: parent.radius
                            color: Colors.red
                            visible: (Pipewire.defaultAudioSink?.audio.volume ?? 0) > 1
                        }
                    }

                    Text {
                        text: Math.round((Pipewire.defaultAudioSink?.audio.volume ?? 0) * 100) + "%"
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
