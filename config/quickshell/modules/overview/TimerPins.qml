import Quickshell
import QtQuick
import "../../services"

Scope {
    Variants {
        model: Timers.pinned

        delegate: TimerPin {}
    }
}
