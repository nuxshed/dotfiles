pragma Singleton
import Quickshell
import Quickshell.Services.Notifications
import QtQuick

Singleton {
    id: root
    
    property list<QtObject> notifications: []
    readonly property var popups: notifications.filter(n => n.popup && !n.closed)

    NotificationServer {
        id: server
        keepOnReload: false
        actionsSupported: true
        bodyMarkupSupported: true
        bodyHyperlinksSupported: true
        imageSupported: true

        onNotification: notif => {
            notif.tracked = true
            
            const wrapper = notifComponent.createObject(root, {
                notification: notif,
                popup: true
            })
            root.notifications = [wrapper, ...root.notifications]
        }
    }

    component NotifWrapper: QtObject {
        property Notification notification
        property bool popup: true
        property bool closed: false
        readonly property date createdAt: new Date()
        
        property string summary: ""
        property string body: ""
        property string appName: ""
        property string appIcon: ""
        property string image: ""
        property int urgency: 0
        property var actions: []

        Component.onCompleted: {
            summary = notification?.summary ?? ""
            body = notification?.body ?? ""
            appName = notification?.appName ?? ""
            appIcon = notification?.appIcon ?? ""
            image = notification?.image ?? ""
            urgency = notification?.urgency ?? 0
            actions = notification?.actions ?? []
        }

        property Timer expireTimer: Timer {
            running: popup && !closed
            interval: 5000
            onTriggered: popup = false
        }

        function close() {
            closed = true
            popup = false
            notification?.dismiss()
        }
        
        function lock() {
            expireTimer.stop()
        }
        
        function unlock() {
            if (popup && !closed) {
                expireTimer.restart()
            }
        }
    }

    Component {
        id: notifComponent
        NotifWrapper {}
    }
}
