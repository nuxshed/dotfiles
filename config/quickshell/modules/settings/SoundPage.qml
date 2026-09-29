pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell.Services.Pipewire
import "../../components"
import "../../config"
import "../../services"

Page {
    id: root

    title: "Sound"
    subtitle: Audio.sink ? "Playing through " + Audio.name(Audio.sink) : "No output device"

    PwObjectTracker {
        objects: Audio.sinks.concat(Audio.sources)
    }

    Repeater {
        model: [
            { title: "Output", node: Audio.sink, devices: Audio.sinks },
            { title: "Input", node: Audio.source, devices: Audio.sources }
        ]

        Group {
            id: section

            required property var modelData

            title: modelData.title

            Item {
                width: parent.width
                height: 60

                LevelSlider {
                    anchors.verticalCenter: parent.verticalCenter
                    x: 16
                    width: parent.width - 32 - value.width - 12
                    implicitHeight: 30
                    track: Colors.surface
                    icon: Audio.volumeIcon(section.modelData.node)
                    value: section.modelData.node?.audio?.volume ?? 0
                    muted: section.modelData.node?.audio?.muted ?? false
                    onMoved: v => Audio.setVolume(section.modelData.node, v)
                    onIconClicked: Audio.toggleMute(section.modelData.node)
                }

                Text {
                    id: value
                    anchors.right: parent.right
                    anchors.rightMargin: 16
                    anchors.verticalCenter: parent.verticalCenter
                    width: 34
                    horizontalAlignment: Text.AlignRight
                    text: Math.round((section.modelData.node?.audio?.volume ?? 0) * 100)
                    color: Colors.textDimmed
                    font.pixelSize: 12
                    font.family: Fonts.family
                    font.features: { "tnum": 1 }
                }
            }

            Repeater {
                model: section.modelData.devices

                SettingRow {
                    required property var modelData

                    icon: Audio.deviceIcon(modelData)
                    label: Audio.name(modelData)
                    description: Audio.detail(modelData)
                    clickable: true
                    selected: modelData === section.modelData.node
                    onClicked: Audio.setDefault(modelData)
                }
            }
        }
    }
}
