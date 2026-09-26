import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Layouts
import "blocks"
import "popups"
import "../../config"

Variants {
    model: Quickshell.screens

    PanelWindow {
        id: panelWindow

        required property var modelData

        screen: modelData

        anchors {
            top: true
            bottom: true
            left: true
        }

        implicitWidth: 70
        color: Colors.background
        WlrLayershell.keyboardFocus: timePopout.visible ? WlrKeyboardFocus.OnDemand : WlrKeyboardFocus.None

        Item {
            focus: timePopout.visible
            Keys.onPressed: event => {
                if (event.key === Qt.Key_Tab)
                    timePopout.offset++;
                else if (event.key === Qt.Key_Backtab)
                    timePopout.offset--;
                else if (event.key === Qt.Key_Escape)
                    timePopout.hide();
                else
                    return;
                event.accepted = true;
            }
        }

        function openPopout(popout, block) {
            for (const other of [mediaPopout, networkPopout, batteryPopout, timePopout, workspacePopout])
                if (other !== popout && other.visible)
                    other.hide()

            popout.show(block)
        }

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 10
            spacing: 10

            Workspaces {
                instant: workspacePopout.shown
                onPreview: (item, workspace) => {
                    workspacePopout.workspace = workspace;
                    panelWindow.openPopout(workspacePopout, item);
                }
            }

            Item { Layout.fillHeight: true }

            ColumnLayout {
                Layout.alignment: Qt.AlignHCenter
                spacing: 8

                Media {
                    id: mediaBlock
                    onClicked: panelWindow.openPopout(mediaPopout, mediaBlock)
                }

                Network {
                    id: networkBlock
                    onClicked: panelWindow.openPopout(networkPopout, networkBlock)
                }

                Battery {
                    id: batteryBlock
                    onClicked: panelWindow.openPopout(batteryPopout, batteryBlock)
                }

                Time {
                    id: timeBlock
                    onClicked: panelWindow.openPopout(timePopout, timeBlock)
                }
            }
        }

        MediaPopout {
            id: mediaPopout
            panel: panelWindow
        }

        NetworkPopout {
            id: networkPopout
            panel: panelWindow
        }

        BatteryPopout {
            id: batteryPopout
            panel: panelWindow
        }

        TimePopout {
            id: timePopout
            panel: panelWindow
        }

        WorkspacePopout {
            id: workspacePopout
            panel: panelWindow
        }
    }
}
