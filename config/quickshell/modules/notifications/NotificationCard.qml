import Quickshell
import QtQuick
import QtQuick.Layouts
import "../../config"

Item {
    id: root
    
    required property var notif
    required property bool isFirst
    
    width: contentRect.width
    height: contentRect.height + (isFirst ? 0 : 8)
    
    property bool dismissing: false
    property bool entered: false
    
    readonly property bool hasImage: notif.image.toString().length > 0
    readonly property bool hasIcon: notif.appIcon.toString().length > 0
    
    Rectangle {
        id: contentRect
        
        y: isFirst ? 0 : 8
        width: hasImage ? 340 : 280
        height: contentColumn.height + 16
        radius: 8
        color: Colors.surface
        border.width: 1
        border.color: notif.urgency === 2 ? Colors.textBright : Colors.border
        
        x: entered ? 0 : 400
        
        Component.onCompleted: {
            x = 0
            entered = true
        }
        
        Behavior on x {
            enabled: !mouseArea.pressed && !dismissing
            NumberAnimation {
                duration: 250
                easing.type: Easing.OutCubic
            }
        }
        
        NumberAnimation {
            id: slideOut
            target: contentRect
            property: "x"
            duration: 200
            easing.type: Easing.InCubic
            onFinished: {
                notif.popup = false
                dismissing = false
                contentRect.opacity = 1
            }
        }
        
        NumberAnimation {
            id: fadeOut
            target: contentRect
            property: "opacity"
            to: 0
            duration: 200
        }
        
        MouseArea {
            id: mouseArea
            anchors.fill: parent
            hoverEnabled: true
            
            property real dragStart: 0
            
            onEntered: notif.lock?.()
            onExited: notif.unlock?.()
            
            onPressed: mouse => dragStart = mouse.x
            
            onPositionChanged: mouse => {
                if (pressed) {
                    contentRect.x += mouse.x - dragStart
                }
            }
            
            onReleased: {
                if (Math.abs(contentRect.x) > 100) {
                    dismissing = true
                    slideOut.to = contentRect.x > 0 ? 500 : -500
                    slideOut.start()
                    fadeOut.start()
                } else {
                    contentRect.x = 0
                }
            }
        }
        
        Column {
            id: contentColumn
            
            x: 12
            y: 8
            width: parent.width - 24
            spacing: 6
            
            Row {
                width: parent.width
                height: 20
                spacing: 6
                
                Image {
                    id: appIconImg
                    width: 20
                    height: 20
                    anchors.verticalCenter: parent.verticalCenter
                    source: hasIcon ? Quickshell.iconPath(notif.appIcon) : ""
                    sourceSize: Qt.size(20, 20)
                    visible: hasIcon
                }
                
                Text {
                    width: parent.width - (appIconImg.visible ? appIconImg.width + 6 : 0) - closeButton.width - 6
                    anchors.verticalCenter: parent.verticalCenter
                    text: notif.appName
                    color: Colors.textDimmed
                    font.pixelSize: 9
                    elide: Text.ElideRight
                }
                
                Item { Layout.fillWidth: true }
                
                Rectangle {
                    id: closeButton
                    width: 20
                    height: 20
                    anchors.verticalCenter: parent.verticalCenter
                    radius: 4
                    color: closeMouse.containsMouse ? Colors.surfaceActive : "transparent"
                    
                    Text {
                        anchors.centerIn: parent
                        text: "×"
                        color: Colors.textDimmed
                        font.pixelSize: 14
                    }
                    
                    MouseArea {
                        id: closeMouse
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: notif.close()
                    }
                    
                    Behavior on color {
                        ColorAnimation { duration: 150 }
                    }
                }
            }
            
            Row {
                width: parent.width
                spacing: 10
                
                Rectangle {
                    width: 64
                    height: 64
                    radius: 6
                    color: Colors.surfaceActive
                    visible: hasImage
                    clip: true
                    
                    Image {
                        anchors.fill: parent
                        source: notif.image
                        fillMode: Image.PreserveAspectCrop
                        asynchronous: true
                    }
                }
                
                Column {
                    width: parent.width - (hasImage ? 74 : 0)
                    spacing: 4
                    
                    Text {
                        width: parent.width
                        text: notif.summary
                        color: Colors.textBright
                        font.pixelSize: 11
                        font.bold: true
                        wrapMode: Text.Wrap
                        maximumLineCount: 2
                        elide: Text.ElideRight
                    }
                    
                    Text {
                        width: parent.width
                        text: notif.body
                        color: Colors.text
                        font.pixelSize: 9
                        wrapMode: Text.Wrap
                        maximumLineCount: hasImage ? 3 : 4
                        elide: Text.ElideRight
                        visible: text.length > 0
                        lineHeight: 1.2
                    }
                }
            }
            
            Row {
                width: parent.width
                spacing: 6
                visible: notif.actions.length > 0
                
                Repeater {
                    model: notif.actions
                    
                    Rectangle {
                        required property var modelData
                        
                        width: (contentColumn.width - (notif.actions.length - 1) * 6) / notif.actions.length
                        height: 26
                        radius: 6
                        color: actionMouse.containsMouse ? Colors.surfaceActive : "transparent"
                        border.width: 1
                        border.color: Colors.border
                        
                        Text {
                            anchors.centerIn: parent
                            text: parent.modelData.text
                            color: Colors.text
                            font.pixelSize: 9
                            font.bold: true
                        }
                        
                        MouseArea {
                            id: actionMouse
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: {
                                parent.modelData.invoke()
                                notif.popup = false
                            }
                        }
                        
                        Behavior on color {
                            ColorAnimation { duration: 150 }
                        }
                    }
                }
            }
        }
    }
}