import Quickshell
import Quickshell.Widgets
import QtQuick
import QtQuick.Layouts
import Qt5Compat.GraphicalEffects
import "../../../services"
import "../../../config"

PopupWindow {
    id: mediaPopup
    
    width: 240
    height: Mpris.hasMultiplePlayers ? 350 : 320
    visible: false

    color: "transparent"
    
    property bool isHovered: false
    property var targetBlock: null
    
    Behavior on height {
        NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
    }
    
    MouseArea {
        anchors.fill: parent
        anchors.margins: -30
        hoverEnabled: true
        propagateComposedEvents: true
        onEntered: mediaPopup.isHovered = true
        onExited: mediaPopup.isHovered = false
    }
    
    Rectangle {
        id: popupContent
        anchors.fill: parent
        radius: 8
        color: Colors.background
        border.color: Colors.border
        border.width: 2
        
        opacity: 0
        scale: 0.8
        transformOrigin: Item.Left

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 14
            spacing: 12
            
            // Player switcher pills - only visible with multiple players
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
                        
                        // Compact mode: icon only
                        Image {
                            anchors.centerIn: parent
                            width: 14
                            height: 14
                            source: Quickshell.iconPath(modelData.desktopEntry || modelData.identity.toLowerCase(), "application-x-executable")
                            sourceSize: Qt.size(14, 14)
                            visible: parent.parent.showCompact
                        }
                        
                        // Normal mode: icon + text
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
                                id: playerText
                                text: modelData.identity || "Player"
                                color: index === Mpris.activePlayerIndex ? Colors.textBright : Colors.text
                                font.pixelSize: 9
                                font.bold: index === Mpris.activePlayerIndex
                                anchors.verticalCenter: parent.verticalCenter
                            }
                        }
                        
                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: Mpris.setActivePlayer(index)
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
            
            // Artwork
            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 160
                radius: 8
                color: Colors.surface
                clip: true

                // Special artwork for MUBI titles
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
                
                // Normal artwork
                Image {
                    id: artwork
                    anchors.fill: parent
                    anchors.margins: 8
                    source: Mpris.artworkUrl
                    fillMode: Image.PreserveAspectFit
                    asynchronous: true
                    visible: !Mpris.trackTitle.endsWith("MUBI") && Mpris.artworkUrl !== ""
                }
                
                // Generic fallback icon
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
            
            // Track info
            ColumnLayout {
                Layout.fillWidth: true
                spacing: 2
                
                Text {
                    Layout.fillWidth: true
                    text: Mpris.trackTitle || "No track"
                    color: Colors.textBright
                    font.pixelSize: 12
                    font.bold: true
                    elide: Text.ElideRight
                }
                
                Text {
                    Layout.fillWidth: true
                    text: Mpris.trackArtist || "Unknown artist"
                    color: Colors.textDimmed
                    font.pixelSize: 10
                    elide: Text.ElideRight
                }
            }
            
            // Progress bar
            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 4
                radius: 2
                color: Colors.surface
                
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
            
            // Control buttons
            Row {
                Layout.alignment: Qt.AlignHCenter
                spacing: 16
                
                Rectangle {
                    width: 36
                    height: 36
                    radius: 18
                    // color: Mpris.canGoPrevious ? Colors.surface : Colors.surfaceDisabled
                    color: Colors.surface
                    
                    Text {
                        anchors.centerIn: parent
                        text: "⏮"
                        color: Mpris.canGoPrevious ? Colors.textBright : Colors.textDimmed
                        font.pixelSize: 16
                    }
                    
                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Mpris.canGoPrevious ? Qt.PointingHandCursor : Qt.ArrowCursor
                        enabled: Mpris.canGoPrevious
                        onClicked: Mpris.previous()
                    }
                }
                
                Rectangle {
                    width: 40
                    height: 40
                    radius: 20
                    color: Colors.surface
                    
                    Text {
                        anchors.centerIn: parent
                        text: Mpris.isPlaying ? "⏸" : "▶"
                        color: Colors.textBright
                        font.pixelSize: 18
                    }
                    
                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        onClicked: Mpris.togglePlayPause()
                    }
                }
                
                Rectangle {
                    width: 36
                    height: 36
                    radius: 18
                    // color: Mpris.canGoNext ? Colors.surface : Colors.surfaceDisabled
                    color: Colors.surface
                    
                    Text {
                        anchors.centerIn: parent
                        text: "⏭"
                        color: Mpris.canGoNext ? Colors.textBright : Colors.textDimmed
                        font.pixelSize: 16
                    }
                    
                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Mpris.canGoNext ? Qt.PointingHandCursor : Qt.ArrowCursor
                        enabled: Mpris.canGoNext
                        onClicked: Mpris.next()
                    }
                }
            }
        }
        
        Behavior on opacity {
            NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
        }
        
        Behavior on scale {
            NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
        }
    }
    
    function show(mediaItem) {
        targetBlock = mediaItem
    
        anchor.item = mediaItem
        anchor.gravity = Edges.Right | Edges.Bottom
        anchor.edges = Edges.Right
        anchor.margins.left = 60
        anchor.margins.bottom = 10
        visible = true
        popupContent.opacity = 1
        popupContent.scale = 1
        autoHideTimer.restart()
    }
    
    function hide() {
        popupContent.opacity = 0
        popupContent.scale = 0.8
        hideTimer.start()
    }
    
    Timer {
        id: hideTimer
        interval: 200
        onTriggered: mediaPopup.visible = false
    }
    
    Timer {
        id: autoHideTimer
        interval: 300
        repeat: true
        running: false
        onTriggered: {
            if (visible && targetBlock && !targetBlock.isHovered && !isHovered) {
                hide()
            }
        }
    }
}
