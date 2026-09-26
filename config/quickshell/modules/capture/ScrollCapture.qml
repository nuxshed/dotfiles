import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

PanelWindow {
    id: win

    readonly property rect area: Qt.rect(Capture.scrollRect.x - screen.x, Capture.scrollRect.y - screen.y, Capture.scrollRect.width, Capture.scrollRect.height)
    readonly property bool below: area.y + area.height + pill.height + 24 < height

    screen: Capture.targetScreen ?? Quickshell.screens[0]
    visible: Capture.scrolling
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "qs:scrollcapture"

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    mask: Region {
        item: pill
    }

    Rectangle {
        readonly property real spread: Math.max(win.width, win.height)

        x: win.area.x - spread
        y: win.area.y - spread
        width: win.area.width + spread * 2
        height: win.area.height + spread * 2
        color: "transparent"
        border.width: spread
        border.color: "#000000"
        opacity: 0.35
    }

    Rectangle {
        x: win.area.x - 2
        y: win.area.y - 2
        width: win.area.width + 4
        height: win.area.height + 4
        color: "transparent"
        border.width: 2
        border.color: Colors.primary
        radius: 2
    }

    Rectangle {
        id: pill

        x: Math.max(8, Math.min(win.width - width - 8, win.area.x + (win.area.width - width) / 2))
        y: win.below ? win.area.y + win.area.height + 12 : Math.max(8, win.area.y - height - 12)
        width: row.implicitWidth + 16
        height: 42
        radius: height / 2
        color: Colors.background
        border.color: Colors.border
        border.width: 1

        RowLayout {
            id: row

            anchors.verticalCenter: parent.verticalCenter
            x: 8
            spacing: 10

            Rectangle {
                Layout.leftMargin: 8
                Layout.preferredWidth: 8
                Layout.preferredHeight: 8
                radius: 4
                color: Colors.red

                SequentialAnimation on opacity {
                    running: win.visible
                    loops: Animation.Infinite
                    NumberAnimation { to: 0.3; duration: 700 }
                    NumberAnimation { to: 1; duration: 700 }
                }
            }

            Text {
                text: Capture.scrollFrames > 1 ? `Scroll slowly · ${Capture.scrollFrames} frames` : "Scroll slowly to capture"
                color: Colors.text
                font.pixelSize: 12
                font.family: Fonts.family
            }

            Rectangle {
                Layout.preferredWidth: 28
                Layout.preferredHeight: 28
                radius: 14
                color: cancelArea.containsMouse ? Colors.surfaceActive : Colors.surface

                MaterialIcon {
                    anchors.centerIn: parent
                    text: "close"
                    size: 16
                }

                MouseArea {
                    id: cancelArea
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: Capture.finishScroll(false)
                }
            }

            Rectangle {
                Layout.preferredWidth: done.implicitWidth + 28
                Layout.preferredHeight: 28
                radius: 14
                color: Colors.primary
                opacity: doneArea.containsMouse ? 0.85 : 1

                Text {
                    id: done
                    anchors.centerIn: parent
                    text: "Done"
                    color: Colors.primaryText
                    font.pixelSize: 12
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }

                MouseArea {
                    id: doneArea
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: Capture.finishScroll(true)
                }
            }
        }
    }
}
