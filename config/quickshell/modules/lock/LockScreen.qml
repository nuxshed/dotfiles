import Quickshell.Wayland
import "../../services"

WlSessionLock {
    id: root

    locked: Lock.locked
    onSecureChanged: Lock.secure = secure

    WlSessionLockSurface {
        color: "#000000"

        LockSurface {
            anchors.fill: parent
        }
    }
}
