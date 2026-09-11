import Quickshell
import QtQuick
import QtQuick.Layouts
import Qt5Compat.GraphicalEffects
import "../../../services"
import "../../../config"
import "../../../components"

Popout {
    id: root

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
        spacing: 16

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
                            font.bold: index === Mpris.activePlayerIndex
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

        Rectangle {
            Layout.fillWidth: true
            Layout.preferredHeight: 210
            radius: 16
            color: Colors.surface
            clip: true

            Rectangle {
                anchors.centerIn: parent
                width: 80
                height: 80
                radius: 12
                color: Colors.surface
                visible: Mpris.trackTitle.endsWith("MUBI")

                Image {
                    id: mubiIcon
                    anchors.centerIn: parent
                    width: 56
                    height: 56
                    source: "../../../assets/icons/mubi.svg"
                    sourceSize: Qt.size(56, 56)
                    fillMode: Image.PreserveAspectFit
                    asynchronous: true
                }

                ColorOverlay {
                    anchors.fill: mubiIcon
                    source: mubiIcon
                    color: Colors.subtle
                }
            }

            Image {
                anchors.fill: parent
                anchors.margins: 8
                source: Mpris.artworkUrl
                fillMode: Image.PreserveAspectFit
                asynchronous: true
                visible: !Mpris.trackTitle.endsWith("MUBI") && Mpris.artworkUrl !== ""
            }

            Image {
                anchors.centerIn: parent
                width: 72
                height: 72
                source: "../../../assets/icons/music-notes.svg"
                sourceSize: Qt.size(72, 72)
                visible: !Mpris.trackTitle.endsWith("MUBI") && Mpris.artworkUrl === ""
                fillMode: Image.PreserveAspectFit
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
                font.bold: true
                elide: Text.ElideRight
            }

            Text {
                Layout.fillWidth: true
                text: Mpris.trackArtist || "Unknown artist"
                color: Colors.textDimmed
                font.pixelSize: 11
                elide: Text.ElideRight
            }
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.preferredHeight: 4
            radius: 2
            color: Colors.surfaceActive

            Rectangle {
                width: parent.width * Mpris.progress
                height: parent.height
                radius: 2
                color: Colors.textBright

                Behavior on width {
                    NumberAnimation { duration: 200 }
                }
            }
        }

        Row {
            Layout.alignment: Qt.AlignHCenter
            spacing: 18

            Rectangle {
                width: 40
                height: 40
                radius: 20
                color: Colors.surface

                Text {
                    anchors.centerIn: parent
                    text: "⏮"
                    color: Mpris.canGoPrevious ? Colors.textBright : Colors.textDimmed
                    font.pixelSize: 16
                }

                HoverHandler {
                    cursorShape: Mpris.canGoPrevious ? Qt.PointingHandCursor : Qt.ArrowCursor
                }

                TapHandler {
                    enabled: Mpris.canGoPrevious
                    onTapped: Mpris.previous()
                }
            }

            Rectangle {
                width: 48
                height: 48
                radius: 24
                color: Colors.surface

                Text {
                    anchors.centerIn: parent
                    text: Mpris.isPlaying ? "⏸" : "▶"
                    color: Colors.textBright
                    font.pixelSize: 20
                }

                HoverHandler {
                    cursorShape: Qt.PointingHandCursor
                }

                TapHandler {
                    onTapped: Mpris.togglePlayPause()
                }
            }

            Rectangle {
                width: 40
                height: 40
                radius: 20
                color: Colors.surface

                Text {
                    anchors.centerIn: parent
                    text: "⏭"
                    color: Mpris.canGoNext ? Colors.textBright : Colors.textDimmed
                    font.pixelSize: 16
                }

                HoverHandler {
                    cursorShape: Mpris.canGoNext ? Qt.PointingHandCursor : Qt.ArrowCursor
                }

                TapHandler {
                    enabled: Mpris.canGoNext
                    onTapped: Mpris.next()
                }
            }
        }
    }
}
