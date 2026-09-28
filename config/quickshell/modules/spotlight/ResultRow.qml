pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import "../../components"
import "../../config"
import "../../services"
import "../../services/spotlight"

Item {
    id: root

    required property var item
    required property bool selected
    required property bool showSection
    property bool expanded: false
    property var actions: []
    property int action: 0

    signal clicked
    signal altClicked
    signal actionClicked(int index)

    readonly property string view: root.item.view ?? ""
    readonly property int rowHeight: root.view === "pair" || root.view === "calc" ? 60 : root.view === "define" ? 64 : 48
    readonly property bool hasMenu: root.actions.length > 1 || (root.item.details?.length ?? 0) > 0

    implicitHeight: header.height + card.height + 2

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
        id: card

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.leftMargin: 8
        anchors.rightMargin: 8
        height: root.rowHeight + (root.expanded ? extra.implicitHeight + 8 : 0)
        radius: 10
        clip: true
        color: root.selected ? Colors.surfaceActive : area.containsMouse ? Colors.surface : "transparent"

        Behavior on color {
            ColorAnimation { duration: 120 }
        }

        Behavior on height {
            Anim { duration: 220 }
        }

        MouseArea {
            id: area

            width: parent.width
            height: root.rowHeight
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: (mouse) => {
                if (mouse.modifiers & Qt.AltModifier)
                    root.altClicked()
                else
                    root.clicked()
            }
        }

        Loader {
            width: parent.width
            height: root.rowHeight
            sourceComponent: root.view === "pair" ? pairView : root.view === "calc" ? calcView : root.view === "define" ? defineView : standardView
        }

        ColumnLayout {
            id: extra

            x: 10
            y: root.rowHeight
            width: parent.width - 20
            spacing: 2
            opacity: root.expanded ? 1 : 0

            Behavior on opacity {
                NumberAnimation { duration: 160 }
            }

            Repeater {
                model: root.expanded ? (root.item.details ?? []) : []

                RowLayout {
                    id: detail

                    required property var modelData

                    Layout.fillWidth: true
                    Layout.leftMargin: 38
                    spacing: 10

                    Text {
                        Layout.preferredWidth: 70
                        text: detail.modelData.label
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }

                    Text {
                        Layout.fillWidth: true
                        text: detail.modelData.value
                        color: Colors.textDimmed
                        font.pixelSize: 10
                        font.family: Fonts.family
                        elide: Text.ElideMiddle
                    }
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 1
                Layout.topMargin: 4
                Layout.bottomMargin: 4
                visible: (root.item.details?.length ?? 0) > 0 && root.actions.length > 0
                color: Colors.border
            }

            Repeater {
                model: root.expanded ? root.actions : []

                Rectangle {
                    id: act

                    required property var modelData
                    required property int index

                    Layout.fillWidth: true
                    Layout.preferredHeight: 30
                    radius: 7
                    color: act.index === root.action ? Colors.subtle : actArea.containsMouse ? Colors.surface : "transparent"

                    RowLayout {
                        anchors.fill: parent
                        anchors.leftMargin: 10
                        anchors.rightMargin: 10
                        spacing: 12

                        MaterialIcon {
                            Layout.preferredWidth: 18
                            text: act.modelData.icon ?? "chevron_right"
                            size: 16
                            color: act.index === root.action ? Colors.textBright : Colors.textMuted
                        }

                        Text {
                            Layout.fillWidth: true
                            text: act.modelData.title
                            color: act.index === root.action ? Colors.textBright : Colors.text
                            font.pixelSize: 11
                            font.family: Fonts.family
                            elide: Text.ElideRight
                        }

                        Text {
                            visible: (act.modelData.hint ?? "").length > 0
                            text: act.modelData.hint ?? ""
                            color: Colors.textMuted
                            font.pixelSize: 10
                            font.family: Fonts.family
                        }
                    }

                    MouseArea {
                        id: actArea

                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: root.actionClicked(act.index)
                    }
                }
            }
        }
    }

    component Keycap: Rectangle {
        property alias label: capText.text

        implicitWidth: capText.implicitWidth + 10
        implicitHeight: 18
        radius: 5
        color: "transparent"
        border.color: Colors.outline
        border.width: 1

        Text {
            id: capText

            anchors.centerIn: parent
            color: Colors.textMuted
            font.pixelSize: 9
            font.family: Fonts.family
        }
    }

    Component {
        id: standardView

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 10
            anchors.rightMargin: 12
            spacing: 12

            Item {
                id: slot

                readonly property string thumb: {
                    Thumbs.version
                    return root.item.thumb ? Thumbs.lookup(root.item.thumb) : ""
                }
                readonly property string art: root.item.art ?? ""
                readonly property string favicon: Favicons.path(root.item.favicon ?? "")
                readonly property string themed: (root.item.iconIsImage ?? false) ? Quickshell.iconPath(root.item.icon ?? "", true) : ""
                readonly property bool imaged: slot.art.length > 0 || slot.thumb.length > 0 || slot.favicon.length > 0 || slot.themed.length > 0

                Layout.preferredWidth: 28
                Layout.preferredHeight: 28

                ClippingRectangle {
                    anchors.fill: parent
                    visible: slot.art.length > 0
                    radius: 6
                    color: Colors.subtle

                    Image {
                        anchors.fill: parent
                        source: slot.art
                        sourceSize: Qt.size(64, 64)
                        fillMode: Image.PreserveAspectCrop
                        asynchronous: true
                    }
                }

                ClippingRectangle {
                    anchors.fill: parent
                    visible: slot.art.length === 0 && slot.thumb.length > 0
                    radius: 6
                    color: Colors.subtle

                    Image {
                        anchors.fill: parent
                        source: slot.thumb.length > 0 ? "file://" + slot.thumb : ""
                        sourceSize: Qt.size(64, 64)
                        fillMode: Image.PreserveAspectCrop
                        asynchronous: true
                    }
                }

                Image {
                    anchors.centerIn: parent
                    width: 20
                    height: 20
                    visible: slot.art.length === 0 && slot.thumb.length === 0 && slot.favicon.length > 0
                    source: slot.favicon
                    sourceSize: Qt.size(40, 40)
                    smooth: true
                }

                IconImage {
                    anchors.centerIn: parent
                    implicitSize: root.view === "app" ? 28 : 22
                    visible: slot.art.length === 0 && slot.thumb.length === 0 && slot.favicon.length === 0 && slot.themed.length > 0
                    source: slot.themed
                }

                MaterialIcon {
                    anchors.centerIn: parent
                    visible: !slot.imaged
                    text: (root.item.iconIsImage ?? false) ? (root.item.fallbackIcon ?? "apps") : (root.item.icon ?? "chevron_right")
                    size: 20
                    color: root.selected ? Colors.textBright : Colors.textDimmed
                }
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 1

                Text {
                    Layout.fillWidth: true
                    text: root.item.title ?? ""
                    color: root.selected ? Colors.textBright : Colors.text
                    font.pixelSize: 13
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }

                Text {
                    Layout.fillWidth: true
                    visible: text.length > 0
                    text: root.item.subtitle ?? ""
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                    elide: Text.ElideMiddle
                }
            }

            RowLayout {
                visible: (root.item.badge ?? "").length > 0 || (root.item.dot ?? "").length > 0 || (root.item.live ?? false)
                spacing: 6

                Rectangle {
                    visible: (root.item.live ?? false) || (root.item.dot ?? "").length > 0
                    implicitWidth: 6
                    implicitHeight: 6
                    radius: 3
                    color: (root.item.dot ?? "").length > 0 ? root.item.dot : Colors.green
                }

                Text {
                    visible: text.length > 0
                    text: root.item.badge ?? ""
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                }
            }

            Row {
                visible: (root.item.keys?.length ?? 0) > 0
                spacing: 4

                Repeater {
                    model: root.item.keys ?? []

                    Keycap {
                        required property string modelData

                        label: modelData
                    }
                }
            }

            Keycap {
                visible: root.selected && root.hasMenu && !root.expanded
                label: "Ctrl Space"
            }
        }
    }

    Component {
        id: calcView

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 10
            anchors.rightMargin: 12
            spacing: 12

            Rectangle {
                Layout.preferredWidth: 32
                Layout.preferredHeight: 32
                radius: 8
                color: Colors.primaryContainer

                MaterialIcon {
                    anchors.centerIn: parent
                    text: root.item.icon ?? "functions"
                    size: 18
                    color: Colors.primaryContainerText
                }
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 0

                Text {
                    Layout.fillWidth: true
                    text: root.item.subtitle ?? ""
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }

                Text {
                    Layout.fillWidth: true
                    text: root.item.title ?? ""
                    color: Colors.textBright
                    font.pixelSize: 20
                    font.family: Fonts.family
                    font.weight: Font.Medium
                    elide: Text.ElideRight
                }
            }

            Keycap {
                visible: root.selected && !root.expanded
                label: "↵ Copy"
            }
        }
    }

    component Side: RowLayout {
        id: money

        property var side: ({})
        property bool bright: false

        spacing: 10

        Item {
            Layout.preferredWidth: 30
            Layout.preferredHeight: 30

            Text {
                anchors.centerIn: parent
                visible: (money.side.flag ?? "").length > 0
                text: money.side.flag ?? ""
                font.family: "Noto Color Emoji"
                font.pixelSize: 24
            }

            Rectangle {
                anchors.fill: parent
                visible: (money.side.flag ?? "").length === 0
                radius: 15
                color: Colors.subtle

                Text {
                    anchors.centerIn: parent
                    text: (money.side.code ?? "").substring(0, 1)
                    color: Colors.textBright
                    font.pixelSize: 13
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }
            }
        }

        ColumnLayout {
            spacing: 0

            Text {
                text: money.side.amount ?? ""
                color: money.bright ? Colors.textBright : Colors.text
                font.pixelSize: money.bright ? 18 : 15
                font.family: Fonts.family
                font.weight: money.bright ? Font.Medium : Font.Normal
            }

            Text {
                text: (money.side.code ?? "") + " · " + (money.side.name ?? "")
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
            }
        }
    }

    Component {
        id: pairView

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 12
            anchors.rightMargin: 12
            spacing: 14

            Side {
                side: root.item.from ?? ({})
            }

            MaterialIcon {
                text: "arrow_forward"
                size: 16
                color: Colors.textMuted
            }

            Side {
                side: root.item.to ?? ({})
                bright: true
            }

            Item {
                Layout.fillWidth: true
            }

            ColumnLayout {
                spacing: 3

                Text {
                    Layout.alignment: Qt.AlignRight
                    text: root.item.subtitle ?? ""
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                }

                Keycap {
                    Layout.alignment: Qt.AlignRight
                    visible: root.selected && !root.expanded
                    label: "↵ Copy"
                }
            }
        }
    }

    Component {
        id: defineView

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 10
            anchors.rightMargin: 12
            spacing: 12

            Rectangle {
                Layout.preferredWidth: 32
                Layout.preferredHeight: 32
                Layout.alignment: Qt.AlignTop
                Layout.topMargin: 10
                radius: 8
                color: Colors.surfaceActive

                MaterialIcon {
                    anchors.centerIn: parent
                    text: "book"
                    size: 16
                    color: Colors.textDimmed
                }
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 2

                RowLayout {
                    spacing: 8

                    Text {
                        text: root.item.title ?? ""
                        color: Colors.textBright
                        font.pixelSize: 13
                        font.family: Fonts.family
                        font.weight: Font.Medium
                    }

                    Rectangle {
                        visible: (root.item.pos ?? "").length > 0
                        implicitWidth: posText.implicitWidth + 12
                        implicitHeight: 16
                        radius: 8
                        color: Colors.primaryContainer

                        Text {
                            id: posText

                            anchors.centerIn: parent
                            text: root.item.pos ?? ""
                            color: Colors.primaryContainerText
                            font.pixelSize: 9
                            font.family: Fonts.family
                        }
                    }
                }

                Text {
                    Layout.fillWidth: true
                    text: root.item.subtitle ?? ""
                    color: Colors.text
                    font.pixelSize: 11
                    font.family: Fonts.family
                    wrapMode: Text.WordWrap
                    maximumLineCount: 2
                    elide: Text.ElideRight
                    lineHeight: 1.1
                }
            }

            Keycap {
                Layout.alignment: Qt.AlignTop
                Layout.topMargin: 12
                visible: root.selected && !root.expanded
                label: "↵ Copy"
            }
        }
    }
}
