import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Page {
    id: root

    readonly property var durations: [3, 5, 8, 12]

    title: "Notifications"
    subtitle: Notifications.all.length === 0 ? "Nothing new." : Notifications.all.length + " in the notification centre."

    Group {
        SettingRow {
            icon: "notifications_off"
            label: "Do not disturb"
            description: "Only critical notifications pop up"

            Toggle {
                checked: Notifications.dnd
                onToggled: Notifications.dnd = !Notifications.dnd
            }
        }

        SettingRow {
            icon: "timer"
            label: "Popup duration"
            description: "How long a notification stays on screen"

            Segmented {
                width: 220
                implicitHeight: 32
                items: root.durations.map(d => d + "s")
                currentIndex: Math.max(0, root.durations.indexOf(Settings.notifTimeout))
                onSelected: index => Settings.set("notifTimeout", root.durations[index])
            }
        }

        SettingRow {
            icon: "clear_all"
            label: "Notification centre"
            description: "Hover the island in the top-right corner, or press Super+A"

            Button {
                text: "Clear all"
                opacity: Notifications.all.length > 0 ? 1 : 0.4
                enabled: Notifications.all.length > 0
                onClicked: Notifications.clear()
            }
        }
    }
}
