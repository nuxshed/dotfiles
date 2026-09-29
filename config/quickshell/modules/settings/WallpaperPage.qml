pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import "../../components"
import "../../config"
import "../../services"

Page {
    id: root

    title: "Wallpaper"
    subtitle: Settings.wallpaperDir.replace(Settings.home, "~")

    RowLayout {
        Layout.fillWidth: true
        spacing: 20

        WallpaperThumb {
            Layout.preferredWidth: 300
            Layout.preferredHeight: 188
            radius: 14
            path: Settings.wallpaper
        }

        ColumnLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignBottom
            spacing: 14

            Column {
                Layout.fillWidth: true
                spacing: 3

                Text {
                    width: parent.width
                    text: Wallpapers.list[Wallpapers.index]?.name ?? Settings.wallpaper.slice(Settings.wallpaper.lastIndexOf("/") + 1)
                    color: Colors.textBright
                    font.pixelSize: 15
                    font.family: Fonts.family
                    font.weight: Font.Medium
                    elide: Text.ElideRight
                }

                Text {
                    text: Wallpapers.list.length + " wallpapers"
                    color: Colors.textMuted
                    font.pixelSize: 12
                    font.family: Fonts.family
                }
            }

            Row {
                spacing: 8

                Button {
                    icon: "shuffle"
                    text: "Shuffle"
                    accent: true
                    onClicked: Wallpapers.random()
                }

                Button {
                    icon: "skip_next"
                    text: "Next"
                    onClicked: Wallpapers.step(1)
                }

                Button {
                    icon: "folder_open"
                    text: "Open folder"
                    onClicked: Quickshell.execDetached(["xdg-open", Settings.wallpaperDir])
                }
            }
        }
    }

    Group {
        title: "Library"
        padding: 18

        Flow {
            id: flow

            readonly property int columns: Math.max(2, Math.floor((width + spacing) / (170 + spacing)))
            readonly property real tileWidth: (width - spacing * (columns - 1)) / columns

            width: parent.width
            spacing: 14

            Repeater {
                model: Wallpapers.list

                Item {
                    id: tile

                    required property var modelData
                    readonly property bool applied: Settings.wallpaper === modelData.path

                    width: flow.tileWidth
                    height: thumb.height + 8

                    Rectangle {
                        anchors.fill: thumb
                        anchors.margins: -4
                        radius: 16
                        color: "transparent"
                        border.width: 2
                        border.color: tile.applied ? Colors.primary : area.containsMouse ? Colors.outline : "transparent"

                        Behavior on border.color {
                            ColorAnimation { duration: 140 }
                        }
                    }

                    WallpaperThumb {
                        id: thumb
                        x: 4
                        y: 4
                        width: parent.width - 8
                        height: width * 0.625
                        path: tile.modelData.path
                    }

                    MouseArea {
                        id: area
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: Wallpapers.set(tile.modelData.path)
                    }
                }
            }
        }
    }

    Group {
        title: "Options"

        SettingRow {
            icon: "crop"
            label: "Scaling"
            description: "Fill crops to cover the screen, Fit shows the whole image"

            Segmented {
                width: 160
                implicitHeight: 32
                items: ["Fill", "Fit"]
                currentIndex: Settings.wallpaperFit === "fit" ? 1 : 0
                onSelected: index => Settings.set("wallpaperFit", index === 1 ? "fit" : "fill")
            }
        }

        SettingRow {
            icon: "folder"
            label: "Folder"
            description: "Images in this folder and one level below"

            Field {
                value: Settings.wallpaperDir
                onAccepted: text => Settings.set("wallpaperDir", text.replace(/^~/, Settings.home).replace(/\/$/, ""))
            }
        }
    }
}
