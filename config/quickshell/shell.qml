//@ pragma IconTheme Papirus-Dark

import Quickshell
import "modules/leftpanel"
import "modules/osd"
import "modules/notifications"
import "modules/dialogs"
import "modules/toolbar"
import "modules/capture"

Scope {
    LeftPanel {}

    Variants {
        model: Quickshell.screens

        delegate: NotificationPopups {}
    }

    Volume {}
    Brightness {}

    PasswordDialog {}

    Toolbar {}
    RegionSelector {}
    Annotator {}
    RecordIndicator {}
    Pins {}
}
