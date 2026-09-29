pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Page {
    id: root

    readonly property var engines: ["ddg", "g", "w", "gh"]

    title: "Desktop"
    subtitle: "Bar, screen edges and launcher."

    Group {
        title: "Bar"

        SettingRow {
            icon: "schedule"
            label: "24-hour clock"
            description: Settings.clock24 ? "Shows 17:30" : "Shows 05:30"

            Toggle {
                checked: Settings.clock24
                onToggled: Settings.set("clock24", !Settings.clock24)
            }
        }
    }

    Group {
        title: "Screen edges"

        SettingRow {
            icon: "photo_camera"
            label: "Capture toolbar"
            description: "Hover the bottom edge for screenshots, recording and tools"

            Toggle {
                checked: Settings.toolbar
                onToggled: Settings.set("toolbar", !Settings.toolbar)
            }
        }

        SettingRow {
            icon: "tune"
            label: "Control centre"
            description: "Hover the right edge for volume, brightness and devices"

            Toggle {
                checked: Settings.controlCenter
                onToggled: Settings.set("controlCenter", !Settings.controlCenter)
            }
        }
    }

    Group {
        title: "Spotlight"

        SettingRow {
            icon: "cloud"
            label: "Weather location"
            description: "City name, blank to detect from your IP"

            Field {
                implicitWidth: 200
                value: Settings.weatherLocation
                placeholder: "Automatic"
                onAccepted: text => Settings.set("weatherLocation", text.trim())
            }
        }
    }

    Group {
        title: "Default search engine"

        Repeater {
            model: root.engines

            SettingRow {
                required property string modelData

                icon: SpotlightConfig.bangs[modelData].icon
                label: SpotlightConfig.bangs[modelData].name
                description: SpotlightConfig.bangs[modelData].domain
                clickable: true
                selected: Settings.searchEngine === modelData
                onClicked: Settings.set("searchEngine", modelData)
            }
        }
    }
}
