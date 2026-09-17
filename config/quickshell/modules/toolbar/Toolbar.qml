pragma ComponentBehavior: Bound

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
        readonly property bool revealed: hovered && !Capture.busy
        readonly property real barWidth: row.implicitWidth + 20
        readonly property real barHeight: 42
        readonly property point probe: hover.hovered ? hover.point.position : Qt.point(-1, -1)

        property bool hovered: false
        property int hoveredIndex: -1

        readonly property var actions: [
            {
                icon: "desktop_windows",
                tip: "Full screenshot",
                act: () => Capture.fullscreen()
            },
            {
                icon: "crop_free",
                tip: "Region screenshot",
                act: () => Capture.region("copy")
            },
            {
                icon: "edit",
                tip: "Region & annotate",
                act: () => Capture.region("annotate")
            },
            {
                icon: "picture_in_picture_alt",
                tip: "Pin region",
                act: () => Capture.region("pin")
            },
            {
                icon: "folder_open",
                tip: "Open screenshots",
                act: () => Capture.openDir(Capture.shotDir)
            },
            {
                icon: "videocam",
                tip: "Record screen",
                rec: "screen",
                divider: true,
                act: () => Recorder.kind === "screen" ? Recorder.stop() : Recorder.startScreen("")
            },
            {
                icon: "photo_camera",
                tip: "Record webcam",
                act: () => Quickshell.execDetached(["bash", "-c", "\"$HOME/.bin/cam\" -s -F"])
            },
            {
                icon: "mic",
                tip: "Record voice",
                rec: "voice",
                act: () => Recorder.kind === "voice" ? Recorder.stop() : Recorder.startVoice()
            },
            {
                icon: "folder_special",
                tip: "Open recordings",
                act: () => Capture.openDir(Recorder.videoDir)
            },
            {
                icon: "description",
                tip: "Extract text",
                divider: true,
                act: () => Capture.region("ocr")
            },
            {
                icon: "link",
                tip: "Upload & copy link",
                act: () => Capture.region("upload")
            },
            {
                icon: "colorize",
                tip: "Colour picker",
                act: () => Capture.pickColor()
            },
            {
                icon: "folder",
                tip: "Files",
                divider: true,
                act: () => Files.show()
            },
            {
                icon: "timeline",
                tip: "System monitor",
                act: () => SysMon.toggle()
            },
            {
                icon: "center_focus_strong",
                tip: "Visual intelligence",
                placeholder: true,
                act: () => Capture.notify("Visual intelligence", "Not implemented yet")
            }
        ]

        screen: modelData
        visible: isActive
        exclusiveZone: 0
        color: "transparent"

        implicitWidth: barWidth + 120
        implicitHeight: 108

        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.namespace: "qs:toolbar"

        anchors.bottom: true

        mask: Region {
            item: hitArea
        }

        onRevealedChanged: settle.restart()

        Timer {
            id: dwell
            interval: 110
            onTriggered: win.hovered = true
        }

        Timer {
            id: unhover
            interval: 200
            onTriggered: win.hovered = false
        }

        Timer {
            id: settle
            interval: 260
            onTriggered: if (!hover.hovered)
                unhover.restart()
        }

        Item {
            id: hitArea
            anchors.bottom: parent.bottom
            anchors.horizontalCenter: parent.horizontalCenter
            width: win.barWidth
            height: win.revealed ? win.barHeight : 3
        }

        Item {
            id: barArea

            anchors.bottom: parent.bottom
            anchors.bottomMargin: win.revealed ? 0 : -(win.barHeight + 12)
            anchors.horizontalCenter: parent.horizontalCenter
            width: win.barWidth
            height: win.barHeight

            Behavior on anchors.bottomMargin {
                NumberAnimation {
                    duration: 320
                    easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1]
                }
            }

            Rectangle {
                id: bar

                anchors.bottom: parent.bottom
                anchors.horizontalCenter: parent.horizontalCenter
                width: win.revealed ? win.barWidth : win.barHeight
                height: win.barHeight
                radius: height / 2
                bottomLeftRadius: 0
                bottomRightRadius: 0
                color: Colors.backgroundDeep
                opacity: win.revealed ? 1 : 0

                Behavior on width {
                    NumberAnimation {
                        duration: 300
                        easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1]
                    }
                }

                Behavior on opacity {
                    NumberAnimation {
                        duration: 120
                    }
                }

                RoundCorner {
                    anchors.right: parent.left
                    anchors.bottom: parent.bottom
                    size: 14
                    color: Colors.backgroundDeep
                    corner: RoundCorner.CornerEnum.BottomRight
                }

                RoundCorner {
                    anchors.left: parent.right
                    anchors.bottom: parent.bottom
                    size: 14
                    color: Colors.backgroundDeep
                    corner: RoundCorner.CornerEnum.BottomLeft
                }

                RowLayout {
                    id: row
                    anchors.centerIn: parent
                    spacing: 2

                    Repeater {
                        model: win.actions

                        ToolbarButton {
                            required property var modelData
                            required property int index

                            readonly property bool live: modelData.rec !== undefined && Recorder.kind === modelData.rec

                            Layout.leftMargin: modelData.divider ? 10 : 0

                            icon: modelData.icon
                            tooltip: live ? "Stop recording" : modelData.tip
                            placeholder: modelData.placeholder ?? false
                            divider: modelData.divider ?? false
                            highlighted: live
                            shown: win.revealed
                            delay: 130 + index * 16
                            probe: win.probe

                            onHoveredChanged: {
                                if (hovered)
                                    win.hoveredIndex = index;
                                else if (win.hoveredIndex === index)
                                    win.hoveredIndex = -1;
                            }
                        }
                    }
                }
            }
        }

        Item {
            anchors.fill: parent
            z: 100

            TapHandler {
                onTapped: {
                    if (win.hoveredIndex >= 0)
                        win.actions[win.hoveredIndex].act();
                }
            }

            HoverHandler {
                id: hover

                onHoveredChanged: {
                    if (hovered) {
                        unhover.stop();
                        dwell.restart();
                    } else if (!settle.running) {
                        dwell.stop();
                        unhover.restart();
                    }
                }
            }
        }
    }
}
