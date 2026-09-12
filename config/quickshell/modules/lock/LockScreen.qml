import Quickshell.Wayland
import "../../config"
import "../../services"

WlSessionLock {
    id: root

    locked: Lock.locked

    WlSessionLockSurface {
        color: Colors.backgroundDeep

        LockSurface {
            anchors.fill: parent
        }
    }
}
