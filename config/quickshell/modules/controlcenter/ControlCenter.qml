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
        readonly property int cardWidth: 316
        readonly property int corner: 14
        readonly property int pill: 56
        readonly property var bezier: [0.16, 1, 0.3, 1, 1, 1]

        property bool hovered: false

        screen: modelData
        visible: isActive
        exclusiveZone: 0
        color: "transparent"
        implicitWidth: cardWidth + 40

        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.namespace: "qs:controlcenter"

        anchors {
            top: true
            bottom: true
            right: true
        }

        mask: Region {
            item: hitArea
        }

        onRevealedChanged: {
            Backlight.watching = revealed;
            settle.restart();
        }

        Timer {
            id: dwell
            interval: 140
            onTriggered: win.hovered = true
        }

        Timer {
            id: unhover
            interval: 260
            onTriggered: win.hovered = false
        }

        Timer {
            id: settle
            interval: 320
            onTriggered: if (!hover.hovered)
                unhover.restart()
        }

        Item {
            id: hitArea
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
            width: win.revealed ? win.cardWidth : 2
            height: win.revealed ? content.implicitHeight + 40 + win.corner * 2 : 200
        }

        Item {
            id: drawer

            anchors.right: parent.right
            anchors.rightMargin: win.revealed ? 0 : -(win.cardWidth + win.corner)
            anchors.verticalCenter: parent.verticalCenter
            width: win.cardWidth
            height: card.height + win.corner * 2

            Behavior on anchors.rightMargin {
                NumberAnimation {
                    duration: 240
                    easing.bezierCurve: win.bezier
                }
            }

            Rectangle {
                id: card

                anchors.right: parent.right
                anchors.verticalCenter: parent.verticalCenter
                width: win.revealed ? win.cardWidth : win.pill
                height: win.revealed ? content.implicitHeight + 40 : win.pill * 2
                radius: 28
                topRightRadius: 0
                bottomRightRadius: 0
                color: Colors.backgroundDeep
                opacity: win.revealed ? 1 : 0
                clip: true

                Behavior on width {
                    NumberAnimation {
                        duration: 220
                        easing.bezierCurve: win.bezier
                    }
                }

                Behavior on height {
                    NumberAnimation {
                        duration: 250
                        easing.bezierCurve: win.bezier
                    }
                }

                Behavior on opacity {
                    NumberAnimation { duration: 120 }
                }

                ColumnLayout {
                    id: content

                    x: 20
                    y: 20
                    width: win.cardWidth - 40
                    spacing: 18

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 10

                        Repeater {
                            model: [
                                { node: Audio.sink },
                                { node: Audio.source },
                                { backlight: true }
                            ]

                            PopIn {
                                id: level

                                required property var modelData
                                required property int index

                                readonly property var node: modelData.node ?? null

                                Layout.fillWidth: true
                                shown: win.revealed
                                delay: 40 + index * 30

                                LevelTile {
                                    width: parent.width
                                    shown: level.shown
                                    value: level.modelData.backlight ? Backlight.value : level.node?.audio?.volume ?? 0
                                    muted: level.node?.audio?.muted ?? false
                                    icon: level.modelData.backlight ? "brightness_medium" : Audio.volumeIcon(level.node)
                                    onMoved: v => level.modelData.backlight ? Backlight.set(v) : Audio.setVolume(level.node, v)
                                    onIconClicked: if (level.node) Audio.toggleMute(level.node)
                                }
                            }
                        }
                    }

                    Repeater {
                        model: [
                            { label: "OUTPUT", nodes: Audio.sinks, current: Audio.sink },
                            { label: "INPUT", nodes: Audio.sources, current: Audio.source }
                        ]

                        ColumnLayout {
                            id: section

                            required property var modelData
                            required property int index

                            Layout.fillWidth: true
                            spacing: 4

                            PopIn {
                                Layout.fillWidth: true
                                Layout.bottomMargin: 4
                                shown: win.revealed
                                delay: 110 + section.index * 50

                                Text {
                                    x: 4
                                    text: section.modelData.label
                                    color: Colors.textMuted
                                    font.pixelSize: 9
                                    font.family: Fonts.family
                                    font.weight: Font.Medium
                                    font.letterSpacing: 1
                                }
                            }

                            Repeater {
                                model: section.modelData.nodes

                                PopIn {
                                    id: device

                                    required property var modelData
                                    required property int index

                                    Layout.fillWidth: true
                                    shown: win.revealed
                                    delay: 125 + section.index * 50 + index * 20

                                    DeviceRow {
                                        width: parent.width
                                        node: device.modelData
                                        active: device.modelData === section.modelData.current
                                    }
                                }
                            }
                        }
                    }

                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 12

                        PopIn {
                            Layout.fillWidth: true
                            shown: win.revealed
                            delay: 210

                            Text {
                                x: 4
                                text: "APPS"
                                color: Colors.textMuted
                                font.pixelSize: 9
                                font.family: Fonts.family
                                font.weight: Font.Medium
                                font.letterSpacing: 1
                            }
                        }

                        Repeater {
                            model: Audio.streams.slice(0, 6)

                            PopIn {
                                id: stream

                                required property var modelData
                                required property int index

                                Layout.fillWidth: true
                                shown: win.revealed
                                delay: 225 + index * 20

                                StreamRow {
                                    width: parent.width
                                    node: stream.modelData
                                }
                            }
                        }

                        PopIn {
                            Layout.fillWidth: true
                            visible: Audio.streams.length === 0
                            shown: win.revealed
                            delay: 225

                            Text {
                                x: 4
                                text: "Nothing playing"
                                color: Colors.textMuted
                                font.pixelSize: 11
                                font.family: Fonts.family
                            }
                        }
                    }
                }
            }

            RoundCorner {
                anchors.right: parent.right
                anchors.bottom: card.top
                corner: RoundCorner.CornerEnum.BottomRight
                size: win.corner
                color: Colors.backgroundDeep
                opacity: card.opacity
            }

            RoundCorner {
                anchors.right: parent.right
                anchors.top: card.bottom
                corner: RoundCorner.CornerEnum.TopRight
                size: win.corner
                color: Colors.backgroundDeep
                opacity: card.opacity
            }
        }

        Item {
            anchors.fill: parent

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
