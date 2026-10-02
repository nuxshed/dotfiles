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
        readonly property int margin: 10
        readonly property int islandHeight: 28
        readonly property int contentHeight: 270
        readonly property real panelWidth: content.implicitWidth + 20
        readonly property real panelHeight: contentHeight + 20 + islandHeight
        readonly property real cardWidth: revealed ? panelWidth : live ? island.implicitWidth + 8 : idle.implicitWidth + 28
        readonly property real cardHeight: revealed ? panelHeight : islandHeight
        readonly property var next: Calendar.events.filter(e => !e.allDay && e.end > now && e.start - now < 2 * 3600000).sort((a, b) => a.start - b.start)[0] ?? null

        property bool hovered: false
        property real now: Date.now()

        screen: modelData
        visible: isActive
        exclusiveZone: 0
        color: "transparent"

        implicitWidth: panelWidth + 120
        implicitHeight: panelHeight + margin + 40

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
            interval: 30000
            running: true
            repeat: true
            onTriggered: win.now = Date.now()
        }

        function until(e: var): string {
            const mins = Math.round((e.start - now) / 60000);
            return mins <= 0 ? "now" : mins < 60 ? `in ${mins}m` : `in ${Math.floor(mins / 60)}h ${mins % 60}m`;
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
                anchors.topMargin: win.margin
                x: Math.round((parent.width - width) / 2)
                width: Math.round(animWidth)
                height: Math.round(win.cardHeight)
                radius: win.revealed ? 18 : win.islandHeight / 2
                color: Colors.background
                border.width: 1
                border.color: Colors.border
                clip: true

                Behavior on animWidth {
                    NumberAnimation {
                        duration: 340
                        easing.type: Easing.BezierSpline
                        easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1]
                    }
                }

                Behavior on height {
                    NumberAnimation {
                        duration: 340
                        easing.type: Easing.BezierSpline
                        easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1]
                    }
                }

                Behavior on radius {
                    NumberAnimation {
                        duration: 240
                    }
                }

                MouseArea {
                    anchors.fill: parent
                    onClicked: {
                        if (!win.revealed)
                            Overview.open = true;
                    }
                }

                Row {
                    id: idle

                    x: Math.round((keyScope.width - width) / 2) - card.x
                    anchors.verticalCenter: parent.top
                    anchors.verticalCenterOffset: win.islandHeight / 2
                    spacing: 6
                    visible: !win.live

                    Text {
                        anchors.verticalCenter: parent.verticalCenter
                        text: Qt.formatDate(new Date(win.now), "ddd d MMM")
                        color: win.next ? Colors.textMuted : Colors.textDimmed
                        font.pixelSize: 11
                        font.family: Fonts.family
                        font.weight: Font.Medium
                    }

                    Rectangle {
                        anchors.verticalCenter: parent.verticalCenter
                        visible: win.next !== null
                        width: 6
                        height: 6
                        radius: 3
                        color: win.next?.color ?? Colors.primary
                    }

                    Text {
                        anchors.verticalCenter: parent.verticalCenter
                        visible: win.next !== null
                        text: win.next ? `${win.next.summary} · ${win.until(win.next)}` : ""
                        color: Colors.textBright
                        font.pixelSize: 11
                        font.family: Fonts.family
                        font.weight: Font.Medium
                        elide: Text.ElideRight
                        width: Math.min(implicitWidth, 260)
                    }

                    Privacy {
                        anchors.verticalCenter: parent.verticalCenter
                        visible: shown
                    }
                }

                Island {
                    id: island
                    anchors.top: parent.top
                    x: Math.round((keyScope.width - baseWidth) / 2) - card.x
                    height: win.islandHeight
                    visible: win.live
                    expanded: win.revealed
                }

                RowLayout {
                    id: content

                    x: 10
                    y: 10 + win.islandHeight
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

            Item {
                id: hitArea

                anchors.top: parent.top
                anchors.horizontalCenter: parent.horizontalCenter
                width: win.revealed ? card.width + win.margin * 2 : Math.max(360, card.width + 200)
                height: win.revealed ? card.height + win.margin * 2 : win.margin + win.islandHeight + 18

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
