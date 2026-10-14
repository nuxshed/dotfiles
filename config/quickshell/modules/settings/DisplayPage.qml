import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Page {
    id: root

    title: "Display & lock"
    subtitle: "Screen brightness and the lock screen."

    Component.onCompleted: Backlight.watching = true
    Component.onDestruction: Backlight.watching = false

    Group {
        title: "Brightness"

        Item {
            width: parent.width
            height: 60

            LevelSlider {
                anchors.verticalCenter: parent.verticalCenter
                x: 16
                width: parent.width - 32 - value.width - 12
                implicitHeight: 30
                track: Colors.surface
                icon: "brightness_6"
                value: Backlight.value
                onMoved: v => Backlight.set(Math.max(0.01, v))
            }

            Text {
                id: value
                anchors.right: parent.right
                anchors.rightMargin: 16
                anchors.verticalCenter: parent.verticalCenter
                width: 34
                horizontalAlignment: Text.AlignRight
                text: Math.round(Backlight.value * 100)
                color: Colors.textDimmed
                font.pixelSize: 12
                font.family: Fonts.family
                font.features: { "tnum": 1 }
            }
        }
    }

    Group {
        title: "Lock screen"

        SettingRow {
            icon: "lock"
            label: "Lock now"
            description: "Also locks the password vault"

            Button {
                text: "Lock"
                onClicked: {
                    SettingsApp.open = false;
                    Lock.lock();
                }
            }
        }
    }
}
