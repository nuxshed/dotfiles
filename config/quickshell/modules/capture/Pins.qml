pragma ComponentBehavior: Bound

import Quickshell
import QtQuick
import "../../services"

Scope {
    id: root

    property var items: []
    property int counter: 0

    function add(entry) {
        entry.uid = ++counter
        items = items.concat([entry])
    }

    function remove(uid) {
        items = items.filter(item => item.uid !== uid)
    }

    Connections {
        target: Capture

        function onPinReady(path) {
            root.add({
                type: "pin",
                path: path
            })
        }
    }

    Connections {
        target: Recorder

        function onFinished(path, kind, peaks) {
            root.add({
                type: "player",
                path: path,
                kind: kind,
                peaks: peaks
            })
        }
    }

    Variants {
        model: root.items

        delegate: PinCard {
            id: card
            onDismissed: root.remove(card.modelData.uid)
        }
    }
}
