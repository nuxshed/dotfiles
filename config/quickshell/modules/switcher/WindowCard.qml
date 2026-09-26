import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import Quickshell.Widgets
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    required property var window
    required property bool selected

    readonly property real thumbHeight: 150
    readonly property real thumbWidth: Math.max(110, Math.min(280, thumbHeight * window.size[0] / Math.max(1, window.size[1])))
    readonly property string icon: Switcher.icon(window.class)

    signal clicked

    implicitWidth: thumbWidth + 16
    implicitHeight: layout.implicitHeight + 16
    radius: 14
    color: selected ? Colors.surfaceActive : hover.hovered ? Colors.surface : "transparent"

    Behavior on color {
        ColorAnimation { duration: 120 }
    }

    ColumnLayout {
        id: layout

        anchors.fill: parent
        anchors.margins: 8
        spacing: 8

        ClippingRectangle {
            Layout.preferredWidth: root.thumbWidth
            Layout.preferredHeight: root.thumbHeight
            radius: 8
            color: Colors.surface

            ScreencopyView {
                id: view
                anchors.fill: parent
                captureSource: Switcher.shown ? Switcher.toplevel(root.window.address) : null
                live: true
            }

            IconImage {
                anchors.centerIn: parent
                implicitSize: 40
                visible: !view.hasContent && root.icon !== ""
                source: root.icon ? Quickshell.iconPath(root.icon, true) : ""
            }
        }

        RowLayout {
            Layout.preferredWidth: root.thumbWidth
            spacing: 6

            IconImage {
                implicitSize: 16
                visible: root.icon !== ""
                source: root.icon ? Quickshell.iconPath(root.icon, true) : ""
            }

            Text {
                Layout.fillWidth: true
                text: root.window.title || root.window.class
                color: root.selected ? Colors.textBright : Colors.text
                font.pixelSize: 11
                font.family: Fonts.family
                elide: Text.ElideRight
            }
        }
    }

    HoverHandler {
        id: hover
        cursorShape: Qt.PointingHandCursor
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.clicked()
    }
}
