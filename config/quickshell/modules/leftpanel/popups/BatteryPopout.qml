import QtQuick
import QtQuick.Layouts
import "../../../services"
import "../../../config"
import "../../../components"

Popout {
    id: root

    readonly property string accent: Battery.isCharging ? Colors.batteryCharging
        : Battery.level <= 20 ? Colors.red
        : Battery.status === "Not charging" ? Colors.batteryNotCharging
        : Colors.batteryDischarging

    readonly property var levels: ["Off", "Low", "Med", "High"]

    readonly property var profiles: [
        { name: "Eco", profile: "Quiet", tint: Colors.profileEco },
        { name: "Balanced", profile: "Balanced", tint: Colors.profileBalance },
        { name: "Turbo", profile: "Performance", tint: Colors.profilePower }
    ]

    notch: 18
    contentHeight: column.implicitHeight + 40

    onOpened: {
        Battery.updateBattery()
        Asusctl.updateProfile()
        Asusctl.updateKeyboardBrightness()
        ring.requestPaint()
    }

    ColumnLayout {
        id: column
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 20
        spacing: 18

        RowLayout {
            Layout.fillWidth: true
            spacing: 16

            Item {
                implicitWidth: 62
                implicitHeight: 62

                Canvas {
                    id: ring
                    anchors.fill: parent

                    onPaint: {
                        const ctx = getContext("2d")
                        ctx.reset()

                        const c = width / 2
                        const r = c - 4
                        const p = Math.max(0, Math.min(1, Battery.level / 100))
                        const gap = 0.24
                        const start = -Math.PI / 2
                        const end = start + p * 2 * Math.PI

                        ctx.lineWidth = 6
                        ctx.lineCap = "round"

                        if (p < 0.97) {
                            ctx.beginPath()
                            ctx.arc(c, c, r, end + gap, start + 2 * Math.PI - gap)
                            ctx.strokeStyle = Colors.surfaceActive
                            ctx.stroke()
                        }

                        if (p > 0.02) {
                            ctx.beginPath()
                            ctx.arc(c, c, r, start, end)
                            ctx.strokeStyle = root.accent
                            ctx.stroke()
                        }
                    }
                }

                Connections {
                    target: Battery
                    function onBatteryChanged() { ring.requestPaint() }
                }

                Text {
                    anchors.centerIn: parent
                    text: Battery.level
                    color: Colors.textBright
                    font.pixelSize: 19
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 3

                Text {
                    text: Battery.isCharging ? "Charging" : Battery.status
                    color: root.accent
                    font.pixelSize: 13
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }

                Text {
                    text: Battery.timeRemaining ? Battery.timeRemaining + " left" : "—"
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                }
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            visible: Asusctl.isAvailable
            spacing: 9

            Text {
                text: "PERFORMANCE"
                color: Colors.textMuted
                font.pixelSize: 9
                font.family: Fonts.family
                font.weight: Font.Medium
                font.letterSpacing: 1
            }

            Segmented {
                Layout.fillWidth: true
                items: root.profiles
                currentIndex: Math.max(0, root.profiles.findIndex(p => p.profile === Asusctl.activeProfile))
                onSelected: index => Asusctl.setProfile(root.profiles[index].profile)
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            visible: Asusctl.isAvailable
            spacing: 8

            Text {
                text: "KEYBOARD"
                color: Colors.textMuted
                font.pixelSize: 9
                font.family: Fonts.family
                font.weight: Font.Medium
                font.letterSpacing: 1
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 36
                radius: 12
                color: kbdHover.hovered ? Colors.surfaceActive : Colors.surface

                Behavior on color {
                    ColorAnimation { duration: 150 }
                }

                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: 14
                    anchors.rightMargin: 14

                    Text {
                        text: "Backlight"
                        color: Colors.text
                        font.pixelSize: 11
                        font.family: Fonts.family
                    }

                    Item { Layout.fillWidth: true }

                    Row {
                        spacing: 5

                        Repeater {
                            model: 3

                            Rectangle {
                                required property int index

                                width: 4
                                height: 13
                                radius: 2
                                color: index < root.levels.indexOf(Asusctl.keyboardBrightness) ? Colors.textBright : Colors.subtle

                                Behavior on color {
                                    ColorAnimation { duration: 150 }
                                }
                            }
                        }
                    }
                }

                HoverHandler {
                    id: kbdHover
                    cursorShape: Qt.PointingHandCursor
                }

                TapHandler {
                    acceptedButtons: Qt.LeftButton | Qt.RightButton
                    onTapped: (point, button) => {
                        if (button === Qt.RightButton)
                            Asusctl.prevKeyboardBrightness()
                        else
                            Asusctl.nextKeyboardBrightness()
                    }
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 36
                radius: 12
                color: auraHover.hovered ? Colors.surfaceActive : Colors.surface

                Behavior on color {
                    ColorAnimation { duration: 150 }
                }

                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: 14
                    anchors.rightMargin: 14
                    spacing: 8

                    Text {
                        text: "Aura"
                        color: Colors.text
                        font.pixelSize: 11
                        font.family: Fonts.family
                    }

                    Item { Layout.fillWidth: true }

                    Text {
                        text: Asusctl.getAuraDisplayName(Asusctl.auraMode)
                        color: Colors.textBright
                        font.pixelSize: 11
                        font.family: Fonts.family
                    }

                    Text {
                        text: "›"
                        color: Colors.textMuted
                        font.pixelSize: 13
                        font.family: Fonts.family
                        opacity: auraHover.hovered ? 1 : 0.5

                        Behavior on opacity {
                            NumberAnimation { duration: 150 }
                        }
                    }
                }

                HoverHandler {
                    id: auraHover
                    cursorShape: Qt.PointingHandCursor
                }

                TapHandler {
                    acceptedButtons: Qt.LeftButton | Qt.RightButton
                    onTapped: (point, button) => {
                        if (button === Qt.RightButton)
                            Asusctl.prevAuraMode()
                        else
                            Asusctl.nextAuraMode()
                    }
                }
            }
        }
    }
}
