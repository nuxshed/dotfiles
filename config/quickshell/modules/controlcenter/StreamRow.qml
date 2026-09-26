import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import Quickshell.Services.Pipewire
import "../../components"
import "../../config"
import "../../services"

RowLayout {
    id: root

    required property PwNode node

    readonly property string icon: Audio.appIcon(node)

    spacing: 12

    Item {
        Layout.preferredWidth: 26
        Layout.preferredHeight: 26

        IconImage {
            anchors.fill: parent
            visible: root.icon !== ""
            source: root.icon ? Quickshell.iconPath(root.icon, true) : ""
        }

        MaterialIcon {
            anchors.centerIn: parent
            visible: root.icon === ""
            text: "graphic_eq"
            size: 18
        }
    }

    ColumnLayout {
        Layout.fillWidth: true
        spacing: 6

        RowLayout {
            Layout.fillWidth: true
            spacing: 6

            Text {
                text: Audio.name(root.node)
                color: Colors.text
                font.pixelSize: 11
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            Text {
                Layout.fillWidth: true
                text: Audio.detail(root.node)
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
                elide: Text.ElideRight
            }

            Text {
                text: Math.round((root.node?.audio?.volume ?? 0) * 100) + "%"
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
            }
        }

        LevelSlider {
            Layout.fillWidth: true
            implicitHeight: 8
            value: root.node?.audio?.volume ?? 0
            muted: root.node?.audio?.muted ?? false
            onMoved: v => Audio.setVolume(root.node, v)
        }
    }
}
