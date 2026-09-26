import Quickshell
import Quickshell.Widgets
import QtQuick
import QtQuick.Layouts
import Qt5Compat.GraphicalEffects
import "../../../services"
import "../../../config"
import "../../../components"

Popout {
    id: root

    readonly property bool mubi: Mpris.trackTitle.endsWith("MUBI")

    function time(seconds: real): string {
        const s = Math.max(0, Math.floor(seconds));
        return Math.floor(s / 60) + ":" + String(s % 60).padStart(2, "0");
    }

    component Control: Rectangle {
        id: control

        property string icon
        property int size: 24
        property bool primary: false
        property bool available: true

        signal activated

        Layout.preferredWidth: primary ? 56 : 44
        Layout.preferredHeight: primary ? 56 : 44
        radius: height / 2
        color: primary ? Colors.text : controlArea.containsMouse && available ? Colors.surface : "transparent"
        scale: controlArea.pressed && available ? 0.9 : 1

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        Behavior on scale {
            NumberAnimation { duration: 160; easing.type: Easing.OutBack; easing.overshoot: 3 }
        }

        MaterialIcon {
            anchors.centerIn: parent
            text: control.icon
            size: control.size
            color: control.primary ? Colors.background : control.available ? Colors.textBright : Colors.textMuted
        }

        MouseArea {
            id: controlArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: control.available ? Qt.PointingHandCursor : Qt.ArrowCursor
            onClicked: if (control.available) control.activated()
        }
    }

    notch: 18
    contentHeight: column.implicitHeight + 40

    Behavior on contentHeight {
        NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
    }

    ColumnLayout {
        id: column
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 20
        spacing: 14

        Flow {
            Layout.fillWidth: true
            Layout.preferredHeight: Mpris.hasMultiplePlayers ? implicitHeight : 0
            visible: Mpris.hasMultiplePlayers
            spacing: 4

            readonly property bool showCompact: Mpris.players.length > 3

            Repeater {
                model: Mpris.players

                Rectangle {
                    width: parent.showCompact ? 24 : (playerRow.width + 12)
                    height: 20
                    radius: 10
                    color: index === Mpris.activePlayerIndex ? Colors.surfaceActive : Colors.surface

                    Image {
                        anchors.centerIn: parent
                        width: 14
                        height: 14
                        source: Quickshell.iconPath(modelData.desktopEntry || modelData.identity.toLowerCase(), "application-x-executable")
                        sourceSize: Qt.size(14, 14)
                        visible: parent.parent.showCompact
                    }

                    Row {
                        id: playerRow
                        anchors.centerIn: parent
                        spacing: 4
                        visible: !parent.parent.showCompact

                        Image {
                            width: 12
                            height: 12
                            source: Quickshell.iconPath(modelData.desktopEntry || modelData.identity.toLowerCase(), "application-x-executable")
                            sourceSize: Qt.size(12, 12)
                            anchors.verticalCenter: parent.verticalCenter
                        }

                        Text {
                            text: modelData.identity || "Player"
                            color: index === Mpris.activePlayerIndex ? Colors.textBright : Colors.text
                            font.pixelSize: 9
                            font.family: Fonts.family
                            font.weight: index === Mpris.activePlayerIndex ? Font.Medium : Font.Normal
                            anchors.verticalCenter: parent.verticalCenter
                        }
                    }

                    HoverHandler {
                        cursorShape: Qt.PointingHandCursor
                    }

                    TapHandler {
                        onTapped: Mpris.setActivePlayer(index)
                    }

                    Behavior on color {
                        ColorAnimation { duration: 150 }
                    }

                    Behavior on width {
                        NumberAnimation { duration: 150 }
                    }
                }
            }
        }

        ClippingRectangle {
            Layout.fillWidth: true
            Layout.preferredHeight: width
            radius: 16
            color: Colors.surface

            Image {
                id: art
                anchors.fill: parent
                source: root.mubi ? "" : Mpris.artworkUrl
                sourceSize: Qt.size(width * 2, height * 2)
                fillMode: Image.PreserveAspectCrop
                asynchronous: true
                visible: status === Image.Ready
            }

            Image {
                id: mubiIcon
                anchors.centerIn: parent
                width: 64
                height: 64
                source: "../../../assets/icons/mubi.svg"
                sourceSize: Qt.size(64, 64)
                fillMode: Image.PreserveAspectFit
                visible: false
            }

            ColorOverlay {
                anchors.fill: mubiIcon
                source: mubiIcon
                color: Colors.subtle
                visible: root.mubi
            }

            MaterialIcon {
                anchors.centerIn: parent
                visible: !root.mubi && !art.visible
                text: "music_note"
                size: 64
                color: Colors.subtle
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            spacing: 2

            Text {
                Layout.fillWidth: true
                text: Mpris.trackTitle || "No track"
                color: Colors.textBright
                font.pixelSize: 13
                font.family: Fonts.family
                font.weight: Font.Medium
                elide: Text.ElideRight
            }

            Text {
                Layout.fillWidth: true
                text: Mpris.trackArtist || "Unknown artist"
                color: Colors.textDimmed
                font.pixelSize: 11
                font.family: Fonts.family
                elide: Text.ElideRight
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            spacing: 6

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 4
                radius: 2
                color: Colors.surfaceActive

                Rectangle {
                    width: parent.width * Math.min(1, Mpris.progress)
                    height: parent.height
                    radius: 2
                    color: Colors.textBright

                    Behavior on width {
                        NumberAnimation { duration: 200 }
                    }
                }
            }

            RowLayout {
                Layout.fillWidth: true
                visible: Mpris.length > 0

                Text {
                    text: root.time(Mpris.position)
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                }

                Item {
                    Layout.fillWidth: true
                }

                Text {
                    text: root.time(Mpris.length)
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                }
            }
        }

        RowLayout {
            Layout.alignment: Qt.AlignHCenter
            spacing: 22

            Control {
                icon: "skip_previous"
                available: Mpris.canGoPrevious
                onActivated: Mpris.previous()
            }

            Control {
                icon: Mpris.isPlaying ? "pause" : "play_arrow"
                size: 30
                primary: true
                onActivated: Mpris.togglePlayPause()
            }

            Control {
                icon: "skip_next"
                available: Mpris.canGoNext
                onActivated: Mpris.next()
            }
        }
    }
}
