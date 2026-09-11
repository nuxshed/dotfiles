pragma Singleton
pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Services.Notifications
import QtQuick

Singleton {
    id: root

    property list<NotifData> list: []
    readonly property var popups: list.filter(n => n.popup || n.reaper.running)

    function clear(): void {
        for (const n of list.slice())
            n.close();
    }

    Timer {
        running: root.list.length > 0
        repeat: true
        interval: 10000
        triggeredOnStart: true
        onTriggered: {
            for (const n of root.list)
                n.updateTime();
        }
    }

    NotificationServer {
        keepOnReload: false
        actionsSupported: true
        bodyMarkupSupported: true
        bodyHyperlinksSupported: true
        imageSupported: true

        onNotification: notif => {
            notif.tracked = true;
            const data = notifComp.createObject(root, {
                notification: notif
            });
            data.init();
            root.list = [data, ...root.list];
        }
    }

    Component {
        id: notifComp

        NotifData {}
    }

    component NotifData: QtObject {
        id: data

        required property Notification notification
        property bool popup: true
        property string timeStr: "now"

        readonly property date time: new Date()
        readonly property var actions: notification?.actions ?? []

        property string summary
        property string body
        property string appName
        property string appIcon
        property string image
        property bool critical

        readonly property Connections conn: Connections {
            target: data.notification

            function onClosed(): void {
                data.popup = false;
            }
        }

        readonly property Timer timer: Timer {
            running: true
            interval: 5000
            onTriggered: {
                if (!data.critical)
                    data.popup = false;
            }
        }

        readonly property Timer reaper: Timer {
            interval: 600
            onTriggered: {
                root.list = root.list.filter(n => n !== data);
                data.destroy();
            }
        }

        function init(): void {
            summary = notification.summary;
            body = notification.body;
            appName = notification.appName;
            appIcon = notification.appIcon;
            image = notification.image;
            critical = notification.urgency === NotificationUrgency.Critical;
            updateTime();
        }

        function updateTime(): void {
            const diff = Math.floor((Date.now() - time.getTime()) / 1000);
            if (diff < 60)
                timeStr = "now";
            else if (diff < 3600)
                timeStr = `${Math.floor(diff / 60)}m`;
            else if (diff < 86400)
                timeStr = `${Math.floor(diff / 3600)}h`;
            else
                timeStr = `${Math.floor(diff / 86400)}d`;
        }

        function close(): void {
            popup = false;
            notification?.dismiss();
        }

        onPopupChanged: {
            if (popup)
                return;
            timer.stop();
            reaper.restart();
        }
    }
}
