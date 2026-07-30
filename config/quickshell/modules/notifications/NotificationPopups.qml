import Quickshell
import Quickshell.Wayland
import Quickshell.Widgets
import QtQuick
import "../../services"

PanelWindow {
    id: root
    
    anchors {
        top: true
        right: true
    }
    
    margins {
        top: 10
        right: 10
    }
    
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "notifications"
    WlrLayershell.exclusiveZone: 0
    
    color: "transparent"
    
    width: notifColumn.width
    height: notifColumn.height
    visible: Notifications.popups.length > 0

    Column {
        id: notifColumn
        spacing: 0

        Repeater {
            model: ScriptModel {
                values: Notifications.popups
            }

            delegate: NotificationCard {
                required property var modelData
                required property int index
                notif: modelData
                isFirst: index === 0
            }
        }
    }
}
