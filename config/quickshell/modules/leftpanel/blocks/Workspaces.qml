import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland

import "../../../services"
import "../../../config"

Rectangle {
    id: root

    property bool instant: false
    property int activeId: Hyprland.focusedWorkspace?.id ?? 1
    property Item activeItem: null
    property int lastId: 0
    property bool down: true

    readonly property var bezier: [0.2, 0.9, 0.3, 1, 1, 1]

    signal preview(Item item, int workspace)

    color: Colors.surface
    radius: 6
    width: 44
    implicitHeight: mainLayout.implicitHeight + 10

    Component.onCompleted: lastId = activeId

    onActiveIdChanged: {
        down = activeId > lastId;
        lastId = activeId;
    }

    Connections {
        target: Hyprland

        function onFocusedWorkspaceChanged() {
            if (Hyprland.focusedWorkspace)
                root.activeId = Hyprland.focusedWorkspace.id;
        }

        function onRawEvent(event) {
            if (event.name === "workspacev2")
                root.activeId = parseInt(event.data);
            else if (event.name === "focusedmonv2")
                root.activeId = parseInt(event.data.split(",")[1]);
        }
    }

    Rectangle {
        id: highlight

        property real from: root.activeItem ? mainLayout.y + root.activeItem.y : 0
        property real to: root.activeItem ? mainLayout.y + root.activeItem.y + root.activeItem.height : 0

        anchors.horizontalCenter: parent.horizontalCenter
        y: from
        width: 30
        height: Math.max(0, to - from)
        radius: 6
        color: Colors.workspaceActive
        visible: root.activeItem !== null

        Behavior on from {
            NumberAnimation {
                duration: root.down ? 380 : 240
                easing.bezierCurve: root.bezier
            }
        }

        Behavior on to {
            NumberAnimation {
                duration: root.down ? 240 : 380
                easing.bezierCurve: root.bezier
            }
        }
    }

    ColumnLayout {
        id: mainLayout
        anchors.centerIn: parent
        width: parent.width
        spacing: 10
        
        Repeater {
            model: Hyprland.workspaces
            
            Rectangle {
                id: workspaceItem
                Layout.preferredWidth: 30
                Layout.preferredHeight: windows.length > 0 ? iconColumn.implicitHeight + 12 : 30
                Layout.alignment: Qt.AlignHCenter

                readonly property int wsId: modelData.id
                readonly property bool active: wsId === root.activeId
                readonly property var windows: HyprlandData.appsByWorkspace[wsId] ?? []
                property bool isHovered: false

                color: "transparent"
                radius: 6

                onActiveChanged: if (active)
                    root.activeItem = workspaceItem
                Component.onCompleted: if (active)
                    root.activeItem = workspaceItem

                Behavior on Layout.preferredHeight {
                    NumberAnimation {
                        duration: 350
                        easing.type: Easing.OutBack
                    }
                }

                Text {
                    anchors.centerIn: parent
                    text: modelData.name || modelData.id
                    color: workspaceItem.active ? Colors.workspaceTextActive : Colors.workspaceTextInactive
                    font.pixelSize: 11
                    font.family: Fonts.family
                    font.weight: Font.Medium
                    visible: workspaceItem.windows.length === 0

                    Behavior on color {
                        ColorAnimation { duration: 200 }
                    }
                }

                ColumnLayout {
                    id: iconColumn
                    anchors.centerIn: parent
                    spacing: 8
                    visible: workspaceItem.windows.length > 0

                    Repeater {
                        model: workspaceItem.windows
                        
                        Image {
                            Layout.preferredWidth: 18
                            Layout.preferredHeight: 18
                            fillMode: Image.PreserveAspectFit
                            source: Quickshell.iconPath(DesktopEntries.heuristicLookup(modelData)?.icon ?? modelData, "image-missing")
                        }
                    }
                }

                Timer {
                    id: dwell
                    interval: 120
                    onTriggered: root.preview(workspaceItem, modelData.id)
                }

                MouseArea {
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onEntered: {
                        workspaceItem.isHovered = true;
                        if (root.instant)
                            root.preview(workspaceItem, modelData.id);
                        else
                            dwell.restart();
                    }
                    onExited: {
                        workspaceItem.isHovered = false;
                        dwell.stop();
                    }
                    onClicked: Hyprland.dispatch(`hl.dsp.focus({ workspace = ${modelData.id} })`)
                }
            }
        }
    }
}
