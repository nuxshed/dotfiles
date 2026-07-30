import Quickshell
import QtQuick
import Qt5Compat.GraphicalEffects
import "../../../config"
import "../../../services"

Item {
    id: networkItem
    width: 44
    height: 44

    readonly property int maxBars: 4
    
    signal clicked()

    Rectangle {
        anchors.fill: parent
        radius: 6
        color: Colors.surface

        Row {
            anchors.centerIn: parent
            anchors.verticalCenterOffset: 0
            spacing: 3
            visible: !Network.isWiredConnected && Network.signalStrength > 0

            Repeater {
                model: maxBars

                Rectangle {
                    width: 4
                    height: 6 + (index * 4)
                    radius: 1
                    color: index < Network.signalLevel ? Colors.textBright : Colors.textBright
                    opacity: index < Network.signalLevel ? 1 : 0.2
                    anchors.bottom: parent.bottom

                    Behavior on color {
                        ColorAnimation { duration: 150 }
                    }

                    Behavior on opacity {
                        NumberAnimation { duration: 150 }
                    }
                }
            }
        }

        Image {
            anchors.centerIn: parent
            width: 20
            height: 20
            source: "../../../assets/icons/network.svg"
            sourceSize: Qt.size(20, 20)
            visible: Network.isWiredConnected

            ColorOverlay {
                anchors.fill: parent
                source: parent
                color: Colors.textDimmed
            }

            Behavior on opacity {
                NumberAnimation { duration: 150 }
            }
        }

        Image {
            anchors.centerIn: parent
            width: 20
            height: 20
            source: "../../../assets/icons/wifi-x.svg"
            sourceSize: Qt.size(20, 20)
            visible: !Network.isWiredConnected && Network.signalStrength === 0

            ColorOverlay {
                anchors.fill: parent
                source: parent
                color: Colors.textDimmed
            }

            Behavior on opacity {
                NumberAnimation { duration: 150 }
            }
        }
    }
    
    property bool isHovered: false
    
    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        hoverEnabled: true
        onEntered: {
            networkItem.isHovered = true
            networkItem.clicked()
        }
        onExited: {
            networkItem.isHovered = false
        }
    }
}
