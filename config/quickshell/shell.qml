//@ pragma IconTheme Papirus-Dark

import Quickshell
import "modules/leftpanel"
import "modules/osd"
import "modules/notifications"

Scope {
    LeftPanel {}

    Variants {
        model: Quickshell.screens
        
        delegate: NotificationPopups {
            screen: modelData
        }
    }

    Volume {}
    Brightness {}
}