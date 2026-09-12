pragma Singleton
pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Services.Notifications
import QtQuick

Singleton {
    id: root

    readonly property int maxStored: 50
    readonly property int expireDelay: 5000
    readonly property int exitDelay: 600

    property list<NotifData> list: []
    property int holds: 0

    readonly property bool paused: holds > 0
    readonly property var all: list.slice()
    readonly property var popups: list.filter(n => n.popup || n.exit.running)

    function hold(): void {
        holds++;
    }

    function release(): void {
        holds = Math.max(0, holds - 1);
    }

    function clear(): void {
        for (const n of list.slice())
            n.dismiss();
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
            const data = notifComp.createObject(root, {
                notification: notif
            });
            data.init();
            root.list = [data, ...root.list];

            for (const old of root.list.slice(root.maxStored))
                old.dismiss();
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
        property bool closing: false
        property bool appClosed: false
        property string timeStr: "now"

        property int remaining: root.expireDelay
        property real resumedAt: 0

        readonly property date time: new Date()
        readonly property var actions: notification?.actions ?? []

        property string summary
        property string body
        property string appName
        property string appIcon
        property string image
        property bool critical

        readonly property RetainableLock lock: RetainableLock {
            object: data.notification
            locked: true

            onDropped: {
                data.appClosed = true;
                data.popup = false;
            }
        }

        readonly property Timer timer: Timer {
            interval: Math.max(1, data.remaining)
            running: data.popup && !data.critical && !data.closing && !root.paused

            onTriggered: data.popup = false

            onRunningChanged: {
                if (running)
                    data.resumedAt = Date.now();
                else if (data.resumedAt > 0) {
                    data.remaining = Math.max(0, data.remaining - (Date.now() - data.resumedAt));
                    data.resumedAt = 0;
                }
            }
        }

        readonly property Timer exit: Timer {
            interval: root.exitDelay
        }

        readonly property Timer reaper: Timer {
            interval: root.exitDelay
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

        function dismiss(): void {
            if (closing)
                return;
            closing = true;
            popup = false;
            if (!appClosed)
                notification?.dismiss();
            lock.locked = false;
            reaper.restart();
        }

        onPopupChanged: {
            if (!popup)
                exit.restart();
        }
    }
}
