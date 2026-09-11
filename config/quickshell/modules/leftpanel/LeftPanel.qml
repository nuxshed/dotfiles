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

        function openPopout(popout, block) {
            for (const other of [mediaPopout, networkPopout, batteryPopout])
                if (other !== popout && other.visible)
                    other.hide()

            popout.show(block)
        }

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 10
            spacing: 10

            Workspaces {}

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
    }
}
