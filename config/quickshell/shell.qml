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
import "modules/sysmon"
import "modules/notes"
import "modules/overview"
import "modules/calendar"
import "modules/focus"
import "modules/booth"
import "modules/switcher"
import "modules/emoji"
import "modules/controlcenter"
import "modules/background"
import "modules/pickers"
import "modules/settings"

Scope {
    Wallpaper {}
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
    Pins {}
    LockScreen {}
    SpotlightPanel {}
    FilesPanel {}
    PreviewPanel {}
    SysMonPanel {}
    MaintenancePanel {}
    NotesPanel {}
    OverviewPanel {}
    TimerPins {}
    CalendarWindow {}
    FocusWindow {}
    BoothPanel {}
    SwitcherPanel {}
    EmojiPanel {}
    ControlCenter {}
    ScrollCapture {}
    PickerPanel {}
    SettingsWindow {}
    Ipc {}
}
