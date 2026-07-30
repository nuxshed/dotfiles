import Quickshell
import Quickshell.Io
import QtQuick
import "../../../services"
import "../../../config"

Item {
    id: batteryItem
    width: 44
    height: 44
    
    property int batteryLevel: Battery.level
    property bool isCharging: Battery.isCharging
    property string batteryStatus: Battery.status
    
    signal clicked()
    
    property bool isHovered: false
    
    Rectangle {
        anchors.fill: parent
        radius: 6
        color: Colors.surface
        
        Canvas {
            id: canvas
            anchors.fill: parent
            anchors.margins: 8
            
            onPaint: {
                var ctx = getContext("2d")
                ctx.clearRect(0, 0, width, height)
                
                var centerX = width / 2
                var centerY = height / 2
                var radius = Math.min(width, height) / 2 - 4
                var startAngle = -Math.PI / 2
                var endAngle = startAngle + (batteryLevel / 100) * 2 * Math.PI
                
                ctx.beginPath()
                ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI)
                ctx.strokeStyle = Colors.border
                ctx.lineWidth = 6
                ctx.stroke()
                
                ctx.beginPath()
                ctx.arc(centerX, centerY, radius, startAngle, endAngle)
                ctx.strokeStyle = Colors.text
                ctx.lineWidth = 6
                ctx.stroke()
            }
            
            Connections {
                target: Battery
                function onBatteryChanged() {
                    canvas.requestPaint()
                }
            }
            
            Component.onCompleted: canvas.requestPaint()
        }
    }
    
    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        hoverEnabled: true
        onEntered: {
            batteryItem.isHovered = true
            batteryItem.clicked()
        }
        onExited: {
            batteryItem.isHovered = false
        }
    }
}

