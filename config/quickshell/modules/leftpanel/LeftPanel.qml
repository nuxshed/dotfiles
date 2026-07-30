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
                     onClicked: {
                         if (powerPopup.visible) powerPopup.hide()
                         mediaPopup.show(mediaBlock)
                     }
                 }
                 
                 Network {
                     id: networkBlock
                     onClicked: {
                         if (powerPopup.visible) powerPopup.hide()
                     }
                 }
                 
                 Battery {
                     id: batteryBlock
                     onClicked: {
                         powerPopup.show(batteryBlock)
                     }
                 }
                 Time {
                    id: timeBlock
                }
             }
         }
         
         MediaPopup {
             id: mediaPopup
         }
         
         Power {
             id: powerPopup
         }
     }
 }