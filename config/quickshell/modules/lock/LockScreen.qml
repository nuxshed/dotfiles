import Quickshell.Wayland
import "../../config"
import "../../services"

WlSessionLock {
    id: root

    locked: Lock.locked
    onSecureChanged: Lock.secure = secure

    WlSessionLockSurface {
        color: Colors.backgroundDeep

        LockSurface {
            anchors.fill: parent
        }
    }
}
