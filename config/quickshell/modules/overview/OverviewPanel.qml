pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland
import "../../components"
import "../../config"
import "../../services"

Variants {
    model: Quickshell.screens

    PanelWindow {
        id: win

        required property var modelData

        readonly property bool isActive: Hyprland.focusedMonitor?.name === modelData.name
        readonly property bool live: Activities.any
        readonly property bool revealed: (hovered || Overview.open) && !Capture.busy
        readonly property int islandHeight: 34
        readonly property int contentHeight: 270
        readonly property real panelWidth: content.implicitWidth + 20
        readonly property real panelHeight: contentHeight + 20 + (live ? islandHeight : 0)
        readonly property real cardWidth: revealed ? panelWidth : live ? island.implicitWidth + 8 : 120
        readonly property real cardHeight: revealed ? panelHeight : live ? islandHeight : 0

        property bool hovered: false

        screen: modelData
        visible: isActive
        exclusiveZone: 0
        color: "transparent"

        implicitWidth: panelWidth + 120
        implicitHeight: panelHeight + 40

        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.namespace: "qs:overview"
        WlrLayershell.keyboardFocus: revealed ? WlrKeyboardFocus.OnDemand : WlrKeyboardFocus.None

        anchors.top: true

        mask: Region {
            item: hitArea
        }

        onRevealedChanged: {
            settle.restart();
            if (revealed)
                keyScope.forceActiveFocus();
        }

        Timer {
            id: dwell
            interval: 110
            onTriggered: win.hovered = true
        }

        Timer {
            id: unhover
            interval: 200
            onTriggered: {
                win.hovered = false;
                Overview.open = false;
            }
        }

        Timer {
            id: settle
            interval: 260
            onTriggered: if (win.hovered && !hover.hovered)
                unhover.restart()
        }

        Item {
            id: keyScope
            anchors.fill: parent
            focus: true

            Keys.onPressed: event => {
                if (event.key === Qt.Key_Escape) {
                    win.hovered = false;
                    Overview.open = false;
                } else if (event.key === Qt.Key_Tab)
                    Overview.cycle(1);
                else if (event.key === Qt.Key_Backtab)
                    Overview.cycle(-1);
                else
                    return;
                event.accepted = true;
            }

            Rectangle {
                id: card

                property real animWidth: win.cardWidth

                anchors.top: parent.top
                x: Math.round((parent.width - width) / 2)
                width: Math.round(animWidth)
                height: Math.round(win.cardHeight)
                radius: 18
                topLeftRadius: 0
                topRightRadius: 0
                color: Colors.backgroundDeep
                opacity: win.revealed || win.live ? 1 : 0
                clip: true

                Behavior on animWidth {
                    NumberAnimation {
                        duration: 320
                        easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1]
                    }
                }

                Behavior on height {
                    NumberAnimation {
                        duration: 320
                        easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1]
                    }
                }

                Behavior on opacity {
                    NumberAnimation {
                        duration: 120
                    }
                }

                MouseArea {
                    anchors.fill: parent
                }

                Island {
                    id: island
                    anchors.top: parent.top
                    anchors.horizontalCenter: parent.horizontalCenter
                    height: win.islandHeight
                    visible: win.live
                    expanded: win.revealed
                }

                RowLayout {
                    id: content

                    x: 10
                    y: 10 + (win.live ? win.islandHeight : 0)
                    height: win.contentHeight
                    spacing: 10

                    NavRail {
                        Layout.fillHeight: true
                        shown: win.revealed
                    }

                    RowLayout {
                        visible: Overview.tab === "calendar"
                        Layout.fillHeight: true
                        spacing: 10

                        Reveal {
                            Layout.fillHeight: true
                            Layout.preferredWidth: 290
                            shown: win.revealed
                            delay: 60

                            CalendarCard {
                                anchors.fill: parent
                            }
                        }

                        Reveal {
                            Layout.fillHeight: true
                            Layout.preferredWidth: 290
                            shown: win.revealed
                            delay: 110

                            AgendaCard {
                                anchors.fill: parent
                            }
                        }
                    }

                    RowLayout {
                        visible: Overview.tab === "focus"
                        Layout.fillHeight: true
                        spacing: 10

                        Reveal {
                            Layout.fillHeight: true
                            Layout.preferredWidth: 290
                            shown: win.revealed
                            delay: 60

                            PomodoroCard {
                                anchors.fill: parent
                            }
                        }

                        Reveal {
                            Layout.fillHeight: true
                            Layout.preferredWidth: 290
                            shown: win.revealed
                            delay: 110

                            TimersCard {
                                anchors.fill: parent
                            }
                        }
                    }

                    Reveal {
                        visible: Overview.tab === "tasks"
                        Layout.fillHeight: true
                        Layout.preferredWidth: 590
                        shown: win.revealed
                        delay: 60

                        TasksCard {
                            anchors.fill: parent
                        }
                    }

                    RowLayout {
                        visible: Overview.tab === "screentime"
                        Layout.fillHeight: true
                        spacing: 10

                        Reveal {
                            Layout.fillHeight: true
                            Layout.preferredWidth: 290
                            shown: win.revealed
                            delay: 60

                            ScreenTimeToday {
                                anchors.fill: parent
                            }
                        }

                        Reveal {
                            Layout.fillHeight: true
                            Layout.preferredWidth: 290
                            shown: win.revealed
                            delay: 110

                            ScreenTimeWeek {
                                anchors.fill: parent
                            }
                        }
                    }
                }
            }

            RoundCorner {
                anchors.right: card.left
                anchors.rightMargin: -1
                anchors.top: parent.top
                size: 14
                color: Colors.backgroundDeep
                corner: RoundCorner.CornerEnum.TopRight
                opacity: card.opacity
            }

            RoundCorner {
                anchors.left: card.right
                anchors.leftMargin: -1
                anchors.top: parent.top
                size: 14
                color: Colors.backgroundDeep
                corner: RoundCorner.CornerEnum.TopLeft
                opacity: card.opacity
            }

            Item {
                id: hitArea

                anchors.top: parent.top
                anchors.horizontalCenter: parent.horizontalCenter
                width: win.revealed || win.live ? card.width + 28 : win.panelWidth
                height: win.revealed || win.live ? card.height : 3

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
}
