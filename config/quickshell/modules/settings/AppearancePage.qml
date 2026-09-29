pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Page {
    id: root

    title: "Appearance"
    subtitle: "Colours are shared by every part of the shell. Wallpaper derives an accent from your current wallpaper."

    Group {
        title: "Theme"
        padding: 18

        Flow {
            id: flow

            readonly property int columns: Math.max(2, Math.floor((width + spacing) / (150 + spacing)))
            readonly property real tileWidth: (width - spacing * (columns - 1)) / columns

            width: parent.width
            spacing: 16

            Repeater {
                model: Themes.list

                Item {
                    id: tile

                    required property var modelData
                    readonly property bool applied: Settings.theme === modelData.id

                    width: flow.tileWidth
                    height: swatch.height + 28

                    Rectangle {
                        anchors.fill: swatch
                        anchors.margins: -4
                        radius: 16
                        color: "transparent"
                        border.width: 2
                        border.color: tile.applied ? Colors.primary : area.containsMouse ? Colors.outline : "transparent"

                        Behavior on border.color {
                            ColorAnimation { duration: 140 }
                        }
                    }

                    ThemeSwatch {
                        id: swatch
                        width: parent.width - 8
                        x: 4
                        y: 4
                        height: width * 0.62
                        themeId: tile.modelData.id
                    }

                    Text {
                        anchors.bottom: parent.bottom
                        anchors.horizontalCenter: parent.horizontalCenter
                        text: tile.modelData.name
                        color: tile.applied ? Colors.textBright : Colors.textDimmed
                        font.pixelSize: 12
                        font.family: Fonts.family
                        font.weight: tile.applied ? Font.Medium : Font.Normal
                    }

                    MouseArea {
                        id: area
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: Settings.set("theme", tile.modelData.id)
                    }
                }
            }
        }
    }

    Group {
        title: "Pickers"

        SettingRow {
            icon: "palette"
            label: "Theme picker"
            description: "Slides up from the bottom edge · Super+Shift+T"

            Button {
                text: "Open"
                onClicked: {
                    SettingsApp.open = false;
                    Pickers.show("theme");
                }
            }
        }

        SettingRow {
            icon: "wallpaper"
            label: "Wallpaper picker"
            description: "Slides up from the bottom edge · Super+Shift+W"

            Button {
                text: "Open"
                onClicked: {
                    SettingsApp.open = false;
                    Pickers.show("wallpaper");
                }
            }
        }
    }
}
