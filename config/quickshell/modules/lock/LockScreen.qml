import Quickshell.Wayland
import "../../config"
import "../../services"

WlSessionLock {
    id: root

    locked: Lock.locked
    onSecureChanged: Lock.secure = secure

    WlSessionLockSurface {
        color: Lock.mode === "bounce" ? "#000000" : Colors.backgroundDeep

        LockSurface {
            anchors.fill: parent
        }
    }
}
