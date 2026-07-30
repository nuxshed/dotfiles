import Quickshell
import Quickshell.Widgets
import QtQuick
import QtQuick.Layouts
import "../../../services"
import "../../../config"

PopupWindow {
    id: powerPopup
    
    width: 260
    height: 260
    visible: false

    color: "transparent"
    
    property bool isHovered: false
    property var targetBlock: null
    
    MouseArea {
        anchors.fill: parent
        anchors.margins: -30
        hoverEnabled: true
        propagateComposedEvents: true
        onEntered: powerPopup.isHovered = true
        onExited: powerPopup.isHovered = false
    }
    
    Rectangle {
        id: popupContent
        anchors.fill: parent
        radius: 8
        color: Colors.background
        border.color: Colors.border
        border.width: 2
        
        opacity: 0
        scale: 0.8
        transformOrigin: Item.Left

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 16
            spacing: 12
            
            // Header - Battery percentage and status
            Row {
                Layout.fillWidth: true
                spacing: 10
                
                Text {
                    text: Battery.level + "%"
                    color: Colors.textBright
                    font.pixelSize: 28
                    font.bold: true
                    anchors.verticalCenter: parent.verticalCenter
                }
                
                ColumnLayout {
                    spacing: 2
                    anchors.verticalCenter: parent.verticalCenter
                    
                    Text {
                        text: Battery.status
                        color: {
                            if (Battery.status === "Charging") return Colors.batteryCharging
                            if (Battery.status === "Not charging") return Colors.batteryNotCharging
                            return Colors.text
                        }
                        font.pixelSize: 11
                        font.bold: true
                    }
                    
                    Text {
                        text: Battery.timeRemaining || "Calculating..."
                        color: Colors.textDimmed
                        font.pixelSize: 9
                        visible: Battery.timeRemaining !== ""
                    }
                }
            }
            
            // Power Profile Section
            ColumnLayout {
                Layout.fillWidth: true
                spacing: 6
                
                Text {
                    text: "Power Profile"
                    color: Colors.textDimmed
                    font.pixelSize: 9
                    font.bold: true
                }
                
                // Tabbed profile selector
                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 36
                    radius: 6
                    color: Colors.surface
                    
                    Row {
                        anchors.fill: parent
                        anchors.margins: 3
                        spacing: 3
                        
                        Rectangle {
                            width: (parent.width - 6) / 3
                            height: parent.height
                            radius: 4
                            color: Asusctl.activeProfile === "Quiet" ? Colors.surfaceActive : "transparent"
                            
                            Text {
                                anchors.centerIn: parent
                                text: "Eco"
                                color: Asusctl.activeProfile === "Quiet" ? Colors.profileEco : Colors.text
                                font.pixelSize: 10
                                font.bold: Asusctl.activeProfile === "Quiet"
                            }
                            
                            MouseArea {
                                anchors.fill: parent
                                cursorShape: Qt.PointingHandCursor
                                onClicked: Asusctl.setProfile("Quiet")
                            }
                            
                            Behavior on color {
                                ColorAnimation { duration: 150 }
                            }
                        }
                        
                        Rectangle {
                            width: (parent.width - 6) / 3
                            height: parent.height
                            radius: 4
                            color: Asusctl.activeProfile === "Balanced" ? Colors.surfaceActive : "transparent"
                            
                            Text {
                                anchors.centerIn: parent
                                text: "Balanced"
                                color: Asusctl.activeProfile === "Balanced" ? Colors.profileBalance : Colors.text
                                font.pixelSize: 10
                                font.bold: Asusctl.activeProfile === "Balanced"
                            }
                            
                            MouseArea {
                                anchors.fill: parent
                                cursorShape: Qt.PointingHandCursor
                                onClicked: Asusctl.setProfile("Balanced")
                            }
                            
                            Behavior on color {
                                ColorAnimation { duration: 150 }
                            }
                        }
                        
                        Rectangle {
                            width: (parent.width - 6) / 3
                            height: parent.height
                            radius: 4
                            color: Asusctl.activeProfile === "Performance" ? Colors.surfaceActive : "transparent"
                            
                            Text {
                                anchors.centerIn: parent
                                text: "Power"
                                color: Asusctl.activeProfile === "Performance" ? Colors.profilePower : Colors.text
                                font.pixelSize: 10
                                font.bold: Asusctl.activeProfile === "Performance"
                            }
                            
                            MouseArea {
                                anchors.fill: parent
                                cursorShape: Qt.PointingHandCursor
                                onClicked: Asusctl.setProfile("Performance")
                            }
                            
                            Behavior on color {
                                ColorAnimation { duration: 150 }
                            }
                        }
                    }
                }
            }
            
            // Keyboard Controls Section
            ColumnLayout {
                Layout.fillWidth: true
                spacing: 6
                
                Text {
                    text: "Keyboard"
                    color: Colors.textDimmed
                    font.pixelSize: 9
                    font.bold: true
                }
                
                // Brightness and Aura in one row
                Row {
                    Layout.fillWidth: true
                    spacing: 6
                    
                    // Brightness control - compact
                    Rectangle {
                        width: (parent.width - 6) / 2
                        height: 32
                        radius: 6
                        color: Colors.surface
                        
                        Row {
                            anchors.fill: parent
                            anchors.margins: 3
                            spacing: 3
                            
                            Rectangle {
                                width: 26
                                height: parent.height
                                radius: 4
                                color: "transparent"
                                
                                Text {
                                    anchors.centerIn: parent
                                    text: "−"
                                    color: Colors.textBright
                                    font.pixelSize: 16
                                }
                                
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: Asusctl.prevKeyboardBrightness()
                                }
                            }
                            
                            Text {
                                width: parent.width - 58
                                height: parent.height
                                text: Asusctl.keyboardBrightness
                                color: Colors.text
                                font.pixelSize: 9
                                horizontalAlignment: Text.AlignHCenter
                                verticalAlignment: Text.AlignVCenter
                                elide: Text.ElideRight
                            }
                            
                            Rectangle {
                                width: 26
                                height: parent.height
                                radius: 4
                                color: "transparent"
                                
                                Text {
                                    anchors.centerIn: parent
                                    text: "+"
                                    color: Colors.textBright
                                    font.pixelSize: 14
                                }
                                
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: Asusctl.nextKeyboardBrightness()
                                }
                            }
                        }
                    }
                    
                    // Aura mode control - compact
                    Rectangle {
                        width: (parent.width - 6) / 2
                        height: 32
                        radius: 6
                        color: Colors.surface
                        
                        Row {
                            anchors.fill: parent
                            anchors.margins: 3
                            spacing: 3
                            
                            Rectangle {
                                width: 26
                                height: parent.height
                                radius: 4
                                color: "transparent"
                                
                                Text {
                                    anchors.centerIn: parent
                                    text: "◀"
                                    color: Colors.textBright
                                    font.pixelSize: 11
                                }
                                
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: Asusctl.prevAuraMode()
                                }
                            }
                            
                            Text {
                                width: parent.width - 58
                                height: parent.height
                                text: Asusctl.getAuraDisplayName(Asusctl.auraMode)
                                color: Colors.text
                                font.pixelSize: 9
                                horizontalAlignment: Text.AlignHCenter
                                verticalAlignment: Text.AlignVCenter
                                elide: Text.ElideRight
                            }
                            
                            Rectangle {
                                width: 26
                                height: parent.height
                                radius: 4
                                color: "transparent"
                                
                                Text {
                                    anchors.centerIn: parent
                                    text: "▶"
                                    color: Colors.textBright
                                    font.pixelSize: 11
                                }
                                
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: Asusctl.nextAuraMode()
                                }
                            }
                        }
                    }
                }
            }
        }
        
        Behavior on opacity {
            NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
        }
        
        Behavior on scale {
            NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
        }
    }
    
    function show(batteryItem) {
        Asusctl.updateProfile()
        Asusctl.updateKeyboardBrightness()
        Battery.updateBattery()
        targetBlock = batteryItem
    
        anchor.item = batteryItem
        anchor.gravity = Edges.Right | Edges.Bottom
        anchor.edges = Edges.Right
        anchor.margins.left = 60
        anchor.margins.bottom = 10
        visible = true
        popupContent.opacity = 1
        popupContent.scale = 1
        autoHideTimer.restart()
    }
    
    function hide() {
        popupContent.opacity = 0
        popupContent.scale = 0.8
        hideTimer.start()
    }
    
    Timer {
        id: hideTimer
        interval: 200
        onTriggered: powerPopup.visible = false
    }
    
    Timer {
        id: autoHideTimer
        interval: 300
        repeat: true
        running: false
        onTriggered: {
            if (visible && targetBlock && !targetBlock.isHovered && !isHovered) {
                hide()
            }
        }
    }
}