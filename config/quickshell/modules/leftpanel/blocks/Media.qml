import QtQuick
import QtQuick.Layouts
import Qt5Compat.GraphicalEffects
import "../../../config"
import "../../../services"

Item {
    id: mediaItem
    implicitWidth: 44
    implicitHeight: 44

    Layout.preferredWidth: visible ? implicitWidth : 0
    Layout.preferredHeight: visible ? implicitHeight : 0
    Layout.alignment: Qt.AlignHCenter

    readonly property bool hasActiveMedia: Mpris.hasActivePlayer
    readonly property url artworkSource: Mpris.artworkUrl
    readonly property bool isPlaying: Mpris.isPlaying
    property bool isHovered: false
    
    signal clicked()

    visible: hasActiveMedia

    Rectangle {
        anchors.fill: parent
        radius: 6
        color: Colors.surface
        clip: true

        Image {
            id: artwork
            anchors.fill: parent
            anchors.margins: 6
            source: artworkSource
            fillMode: Image.PreserveAspectCrop
            asynchronous: true
            visible: artworkSource !== ""
            opacity: isPlaying ? 1.0 : 0.5
            layer.enabled: true
            layer.effect: OpacityMask {
                maskSource: Rectangle {
                    width: artwork.width
                    height: artwork.height
                    radius: 6
                }
            }
            
            Behavior on opacity {
                NumberAnimation { duration: 150 }
            }
        }

        Text {
            anchors.centerIn: parent
            text: "m"
            color: Colors.textBright
            font.pixelSize: 20
            font.bold: true
            visible: artworkSource === ""
            opacity: isPlaying ? 1.0 : 0.5
            
            Behavior on opacity {
                NumberAnimation { duration: 150 }
            }
        }

        // Pause overlay
        Rectangle {
            anchors.centerIn: parent
            width: 24
            height: 24
            radius: 12
            color: Qt.rgba(0, 0, 0, 0)
            visible: !isPlaying

            Text {
                anchors.centerIn: parent
                text: "▶"
                color: Colors.textBright
                font.pixelSize: 14
            }
        }

        MouseArea {
            anchors.fill: parent
            cursorShape: Qt.PointingHandCursor
            hoverEnabled: true
            onEntered: {
                mediaItem.isHovered = true
                mediaItem.clicked()
            }
            onExited: {
                mediaItem.isHovered = false
            }
            onClicked: {
                Mpris.togglePlayPause()
            }
        }
    }
}
