//@ pragma IconTheme Papirus-Dark

import Quickshell
import "modules/leftpanel"
import "modules/osd"
import "modules/notifications"
import "modules/dialogs"
import "modules/toolbar"
import "modules/capture"
import "modules/ipc"
import "modules/lock"
import "modules/spotlight"
import "modules/files"
import "modules/preview"

Scope {
    LeftPanel {}

    Variants {
        model: Quickshell.screens

        delegate: NotificationPanel {}
    }

    Volume {}
    Brightness {}

    PasswordDialog {}

    Toolbar {}
    CursorShield {}
    RegionSelector {}
    Annotator {}
    RecordIndicator {}
    Pins {}
    LockScreen {}
    SpotlightPanel {}
    FilesPanel {}
    PreviewPanel {}
    Ipc {}
}
