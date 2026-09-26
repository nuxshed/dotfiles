pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Widgets
import "../../../services"
import "../../../config"
import "../../../components"

Popout {
    id: root

    property int workspace: 0
    readonly property var data: HyprlandData.workspaceById[workspace] ?? null
    readonly property var monitor: HyprlandData.monitors.find(m => m.id === data?.monitorID) ?? HyprlandData.monitors[0]
    readonly property real mw: (monitor?.width ?? 1920) / (monitor?.scale ?? 1)
    readonly property real mh: (monitor?.height ?? 1080) / (monitor?.scale ?? 1)
    readonly property real scale: preview.width / mw
    readonly property var windows: HyprlandData.windowList.filter(w => w.workspace.id === workspace && w.mapped && !w.hidden)

    contentWidth: 260
    contentHeight: column.implicitHeight + 36

    ColumnLayout {
        id: column

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 18
        spacing: 10

        ClippingRectangle {
            id: preview

            Layout.fillWidth: true
            Layout.preferredHeight: Math.round(root.mh * root.scale)
            radius: 10
            color: Colors.surface

            Repeater {
                model: root.windows

                Rectangle {
                    id: win

                    required property var modelData

                    readonly property string icon: DesktopEntries.heuristicLookup(modelData.class)?.icon ?? modelData.class

                    x: (modelData.at[0] - (root.monitor?.x ?? 0)) * root.scale
                    y: (modelData.at[1] - (root.monitor?.y ?? 0)) * root.scale
                    z: modelData.floating ? 1 : 0
                    width: Math.max(10, modelData.size[0] * root.scale)
                    height: Math.max(10, modelData.size[1] * root.scale)
                    radius: 6
                    color: Colors.surfaceActive
                    border.color: Colors.outline
                    border.width: 1

                    IconImage {
                        anchors.centerIn: parent
                        implicitSize: Math.min(30, parent.width * 0.5, parent.height * 0.5)
                        source: Quickshell.iconPath(win.icon, "application-x-executable")
                    }
                }
            }

            Text {
                anchors.centerIn: parent
                visible: root.windows.length === 0
                text: "Empty"
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }

            MouseArea {
                anchors.fill: parent
                cursorShape: Qt.PointingHandCursor
                onClicked: {
                    Hyprland.dispatch(`hl.dsp.focus({ workspace = ${root.workspace} })`);
                    root.hide();
                }
            }
        }

        RowLayout {
            Layout.fillWidth: true

            Text {
                text: "Workspace " + (root.data?.name ?? root.workspace)
                color: Colors.textBright
                font.pixelSize: 12
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            Item {
                Layout.fillWidth: true
            }

            Text {
                text: root.windows.length === 1 ? "1 window" : root.windows.length + " windows"
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
            }
        }
    }
}
