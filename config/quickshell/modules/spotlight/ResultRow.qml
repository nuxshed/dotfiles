import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import "../../components"
import "../../config"

Item {
    id: root

    required property var item
    required property bool selected
    required property bool showSection

    signal clicked
    signal altClicked

    implicitHeight: (root.showSection ? header.height : 0) + 44

    Text {
        id: header

        visible: root.showSection
        height: visible ? 26 : 0
        x: 14
        text: root.item.section ?? ""
        color: Colors.textMuted
        font.pixelSize: 10
        font.family: Fonts.family
        font.capitalization: Font.AllUppercase
        font.letterSpacing: 0.6
        verticalAlignment: Text.AlignBottom
    }

    Rectangle {
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.leftMargin: 8
        anchors.rightMargin: 8
        height: 40
        radius: 8
        color: root.selected ? Colors.surfaceActive : hover.hovered ? Colors.surface : "transparent"

        Behavior on color {
            ColorAnimation { duration: 120 }
        }

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 10
            anchors.rightMargin: 12
            spacing: 10

            Item {
                Layout.preferredWidth: 20
                Layout.preferredHeight: 20

                IconImage {
                    anchors.fill: parent
                    visible: root.item.iconIsImage ?? false
                    source: (root.item.iconIsImage ?? false) ? Quickshell.iconPath(root.item.icon ?? "", true) : ""
                }

                MaterialIcon {
                    anchors.centerIn: parent
                    visible: !(root.item.iconIsImage ?? false)
                    text: root.item.icon ?? "chevron_right"
                    size: 18
                    color: root.selected ? Colors.textBright : Colors.textDimmed
                }
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 0

                Text {
                    Layout.fillWidth: true
                    text: root.item.title ?? ""
                    color: root.selected ? Colors.textBright : Colors.text
                    font.pixelSize: 12
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }

                Text {
                    Layout.fillWidth: true
                    visible: (root.item.subtitle ?? "").length > 0
                    text: root.item.subtitle ?? ""
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                    elide: Text.ElideMiddle
                }
            }

            Text {
                visible: root.selected && (root.item.altHint ?? "").length > 0
                text: root.item.altHint ?? ""
                color: Colors.textMuted
                font.pixelSize: 9
                font.family: Fonts.family
            }
        }

        HoverHandler {
            id: hover
            cursorShape: Qt.PointingHandCursor
        }

        TapHandler {
            acceptedButtons: Qt.LeftButton
            onTapped: (event) => {
                if (event.modifiers & Qt.AltModifier)
                    root.altClicked()
                else
                    root.clicked()
            }
        }
    }
}
