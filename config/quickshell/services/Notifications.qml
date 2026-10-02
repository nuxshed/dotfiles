pragma Singleton
pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Services.Notifications
import QtQuick
import "../config"

Singleton {
    id: root

    readonly property int maxStored: 100
    readonly property int maxPopups: 3
    readonly property int exitDelay: 400

    property list<Notif> list: []
    property bool dnd: false
    property bool open: false
    property bool replying: false
    property int holds: 0

    readonly property bool paused: holds > 0 || replying
    readonly property var all: list.filter(n => !n.closing)
    readonly property var popups: all.filter(n => n.popup)
    readonly property var unread: all.filter(n => !n.read)
    readonly property var groups: {
        const keys = [];
        for (const n of all)
            if (!keys.includes(n.key))
                keys.push(n.key);
        return keys;
    }
    readonly property var unreadApps: {
        const seen = [];
        for (const n of unread)
            if (!seen.some(s => s.key === n.key))
                seen.push(n);
        return seen;
    }

    function items(key: string): var {
        return all.filter(n => n.key === key);
    }

    function dismissGroup(key: string): void {
        for (const n of items(key))
            n.dismiss();
    }

    function clear(): void {
        for (const n of all)
            n.dismiss();
    }

    function dismissLatest(): void {
        popups[0]?.dismiss();
    }

    function hidePopups(): void {
        for (const n of popups)
            n.popup = false;
    }

    function markRead(): void {
        for (const n of unread)
            n.read = true;
    }

    function toggle(): void {
        open = !open;
    }

    function hold(): void {
        holds++;
    }

    function release(): void {
        holds = Math.max(0, holds - 1);
    }

    function promote(n: Notif): void {
        list = [n, ...list.filter(o => o !== n)];
        for (const p of popups.slice(maxPopups))
            p.popup = false;
    }

    Timer {
        running: root.all.length > 0
        repeat: true
        interval: 30000
        onTriggered: {
            for (const n of root.all)
                n.tick();
        }
    }

    NotificationServer {
        keepOnReload: true
        actionsSupported: true
        bodyMarkupSupported: true
        bodyHyperlinksSupported: true
        imageSupported: true
        inlineReplySupported: true
        persistenceSupported: true

        onNotification: notif => {
            notif.tracked = true;
            const n = notifComp.createObject(root, {
                notification: notif
            });
            n.sync();
            n.read = notif.lastGeneration;
            n.popup = !notif.lastGeneration && (!root.dnd || n.critical);
            root.promote(n);
            for (const old of root.all.slice(root.maxStored))
                old.dismiss();
        }
    }

    Component {
        id: notifComp

        Notif {}
    }

    component Notif: QtObject {
        id: n

        property Notification notification
        property string key
        property string appName
        property string icon
        property string summary
        property string body
        property string image
        property bool critical
        property bool ephemeral
        property bool resident
        property bool hasReply
        property string replyHint
        property bool hasDefault
        property var actions: []
        property date time: new Date()
        property string timeStr: "now"

        property bool popup: false
        property bool read: false
        property bool closing: false
        property real progress: 1

        readonly property NumberAnimation drain: NumberAnimation {
            target: n
            property: "progress"
            from: 1
            to: 0
            duration: Settings.notifTimeout * 1000
            running: n.popup && !n.critical && !n.closing
            paused: running && root.paused
            onFinished: {
                if (n.progress <= 0)
                    n.popup = false;
            }
        }

        readonly property Timer reaper: Timer {
            interval: root.exitDelay
            onTriggered: {
                root.list = root.list.filter(o => o !== n);
                n.destroy();
            }
        }

        readonly property Connections conn: Connections {
            target: n.notification

            function onClosed(): void {
                n.drop();
            }

            function onSummaryChanged(): void {
                n.refresh();
            }

            function onBodyChanged(): void {
                n.refresh();
            }

            function onImageChanged(): void {
                n.sync();
            }

            function onAppIconChanged(): void {
                n.sync();
            }

            function onActionsChanged(): void {
                n.sync();
            }
        }

        function sync(): void {
            const src = notification;
            if (!src)
                return;
            const entry = src.desktopEntry || src.appName;
            key = (entry || "unknown").toLowerCase();
            appName = src.appName || DesktopEntries.heuristicLookup(entry)?.name || "Notification";
            const themed = src.image.startsWith("image://icon/") ? src.image.slice(13) : "";
            icon = src.appIcon || themed || DesktopEntries.heuristicLookup(entry)?.icon || "";
            summary = src.summary;
            body = src.body;
            image = themed ? "" : src.image;
            critical = src.urgency === NotificationUrgency.Critical;
            ephemeral = src.transient;
            resident = src.resident;
            hasReply = src.hasInlineReply;
            replyHint = src.inlineReplyPlaceholder || "Reply";
            hasDefault = src.actions.some(a => a.identifier === "default");
            actions = src.actions.filter(a => a.identifier !== "default" && a.text).map(a => ({
                        id: a.identifier,
                        text: a.text
                    }));
        }

        function refresh(): void {
            sync();
            time = new Date();
            timeStr = "now";
            read = false;
            if (!root.dnd || critical) {
                popup = true;
                drain.restart();
            }
            root.promote(n);
        }

        function tick(): void {
            const s = Math.floor((Date.now() - time.getTime()) / 1000);
            timeStr = s < 60 ? "now" : s < 3600 ? `${Math.floor(s / 60)}m` : s < 86400 ? `${Math.floor(s / 3600)}h` : `${Math.floor(s / 86400)}d`;
        }

        function invoke(id: string): void {
            const action = notification?.actions.find(a => a.identifier === id);
            if (!action)
                return;
            action.invoke();
            if (!resident)
                dismiss();
        }

        function activate(): void {
            if (hasDefault)
                invoke("default");
        }

        function reply(text: string): void {
            if (!hasReply || !notification)
                return;
            notification.sendInlineReply(text);
            if (!resident)
                dismiss();
        }

        function dismiss(): void {
            if (closing)
                return;
            notification?.dismiss();
            drop();
        }

        function drop(): void {
            if (closing)
                return;
            closing = true;
            popup = false;
            reaper.start();
        }

        onPopupChanged: {
            if (!popup && ephemeral && !closing)
                notification?.expire();
        }
    }
}
