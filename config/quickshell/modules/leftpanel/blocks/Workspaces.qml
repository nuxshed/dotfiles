import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland

import "../../../services"
import "../../../config"

Rectangle {
    color: Colors.surface
    radius: 6
    width: 44
    implicitHeight: mainLayout.implicitHeight + 10

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

                readonly property var windows: HyprlandData.windowsForWorkspace(modelData.id)

                color: modelData.id === Hyprland.focusedWorkspace.id ? Colors.workspaceActive : Colors.workspaceInactive
                radius: 6
                
                Behavior on Layout.preferredHeight {
                    NumberAnimation {
                        duration: 350
                        easing.type: Easing.OutBack
                    }
                }

                Text {
                    anchors.centerIn: parent
                    text: modelData.name || modelData.id
                    color: modelData.id === Hyprland.focusedWorkspace.id ? Colors.workspaceTextActive : Colors.workspaceTextInactive
                    font.pixelSize: 11
                    font.bold: true
                    visible: workspaceItem.windows.length === 0
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
                            source: Quickshell.iconPath(modelData.class, "image-missing")
                        }
                    }
                }

                MouseArea {
                    anchors.fill: parent
                    cursorShape: Qt.PointingHandCursor
                    onClicked: Hyprland.dispatch("workspace " + modelData.id)
                }
            }
        }
    }
}
