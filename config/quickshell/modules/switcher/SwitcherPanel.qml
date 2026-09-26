pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import Quickshell.Widgets
import "../../components"
import "../../config"
import "../../services"

PanelWindow {
    id: root

    readonly property bool workspaces: Switcher.mode === "workspaces"

    property string dragAddress: ""
    property string dragIcon: ""
    property point dragPos

    screen: Quickshell.screens.find(s => s.name === Hyprland.focusedMonitor?.name) ?? Quickshell.screens[0]
    visible: Switcher.open
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:switcher"
    WlrLayershell.keyboardFocus: Switcher.open ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

    mask: Region {
        item: Switcher.shown ? backdrop : null
    }

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    onVisibleChanged: {
        if (visible)
            keys.forceActiveFocus();
        dragAddress = "";
    }

    function workspaceAt(pos: point): var {
        for (let i = 0; i < cards.count; i++) {
            const card = cards.itemAt(i);
            if (card && card.contains(card.mapFromItem(null, pos.x, pos.y)))
                return card.workspace ?? null;
        }
        return null;
    }

    Item {
        id: keys

        focus: true
        Keys.onReleased: event => {
            const mod = root.workspaces ? [Qt.Key_Super_L, Qt.Key_Super_R, Qt.Key_Meta] : [Qt.Key_Alt, Qt.Key_AltGr];
            if (mod.includes(event.key) && !root.dragAddress)
                Switcher.commit();
        }
        Keys.onPressed: event => {
            if (event.key === Qt.Key_Escape)
                Switcher.cancel();
            else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter)
                Switcher.commit();
            else if (event.key === Qt.Key_Right || event.key === Qt.Key_Tab || event.key === Qt.Key_L)
                Switcher.move(1);
            else if (event.key === Qt.Key_Left || event.key === Qt.Key_Backtab || event.key === Qt.Key_H)
                Switcher.move(-1);
            else
                return;
            event.accepted = true;
        }
    }

    Rectangle {
        id: backdrop

        anchors.fill: parent
        color: "#000000"
        opacity: Switcher.shown ? 0.3 : 0

        Behavior on opacity {
            NumberAnimation { duration: 150 }
        }

        MouseArea {
            anchors.fill: parent
            onClicked: Switcher.cancel()
        }
    }

    Rectangle {
        id: card

        anchors.centerIn: parent
        width: flow.childrenRect.width + 24
        height: flow.childrenRect.height + 24
        radius: 20
        color: Colors.background
        border.color: Colors.border
        border.width: 1
        opacity: Switcher.shown ? 1 : 0
        scale: Switcher.shown ? 1 : 0.97

        Behavior on opacity {
            Anim { duration: 140 }
        }

        Behavior on scale {
            Anim { duration: 140 }
        }

        MouseArea {
            anchors.fill: parent
        }

        Flow {
            id: flow

            x: 12
            y: 12
            width: root.width * 0.86
            spacing: 6

            Repeater {
                id: cards

                model: Switcher.shown ? Switcher.items : []

                Loader {
                    id: slot

                    required property var modelData
                    required property int index

                    readonly property var workspace: root.workspaces ? modelData : null

                    sourceComponent: root.workspaces ? workspaceCard : windowCard

                    Component {
                        id: windowCard

                        WindowCard {
                            window: slot.modelData
                            selected: slot.index === Switcher.index
                            onClicked: Switcher.select(slot.index)
                        }
                    }

                    Component {
                        id: workspaceCard

                        WorkspaceCard {
                            workspace: slot.modelData
                            selected: slot.index === Switcher.index
                            onClicked: Switcher.select(slot.index)
                            onDragStarted: (address, icon) => {
                                root.dragAddress = address;
                                root.dragIcon = icon;
                            }
                            onDragMoved: pos => {
                                root.dragPos = pos;
                                const target = root.workspaceAt(pos);
                                if (target)
                                    Switcher.index = Switcher.items.indexOf(target);
                            }
                            onDropped: (address, pos) => {
                                const target = root.workspaceAt(pos);
                                if (target && target.id !== slot.modelData.id)
                                    Switcher.moveWindow(address, target.id);
                                root.dragAddress = "";
                            }
                        }
                    }
                }
            }
        }
    }

    Rectangle {
        visible: root.dragAddress !== ""
        x: root.dragPos.x - width / 2
        y: root.dragPos.y - height / 2
        width: 44
        height: 44
        radius: 12
        color: Colors.surfaceActive
        border.color: Colors.outline
        border.width: 1

        IconImage {
            anchors.centerIn: parent
            implicitSize: 26
            source: root.dragIcon ? Quickshell.iconPath(root.dragIcon, true) : ""
        }
    }
}
