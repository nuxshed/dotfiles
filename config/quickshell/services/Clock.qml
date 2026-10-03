pragma Singleton

import QtQuick
import Quickshell

Singleton {
    id: root

    readonly property date now: clock.date
    readonly property string key: Qt.formatDate(clock.date, "yyyy-MM-dd")
    readonly property string tomorrowKey: {
        const d = new Date(clock.date);
        d.setDate(d.getDate() + 1);
        return Qt.formatDate(d, "yyyy-MM-dd");
    }

    property string previous: key

    signal dayChanged(string from, string to)

    onKeyChanged: {
        const from = root.previous;
        root.previous = root.key;
        root.dayChanged(from, root.key);
    }

    SystemClock {
        id: clock
        precision: SystemClock.Minutes
    }
}
