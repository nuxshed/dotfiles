import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Layouts
import "../../services"
import "../../config"

PanelWindow {
    id: root

    visible: Prompt.open
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:prompt"
    WlrLayershell.keyboardFocus: Prompt.open ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    onVisibleChanged: {
        if (visible) {
            input.text = ""
            input.forceActiveFocus()
        }
    }

    Rectangle {
        anchors.fill: parent
        color: "#000000"
        opacity: Prompt.open ? 0.5 : 0

        Behavior on opacity {
            NumberAnimation { duration: 150 }
        }

        TapHandler {
            onTapped: Prompt.close()
        }
    }

    Rectangle {
        id: card

        anchors.centerIn: parent
        width: 380
        height: layout.implicitHeight + 44
        radius: 24
        color: Colors.background
        border.color: Colors.border
        border.width: 1
        opacity: Prompt.open ? 1 : 0
        scale: Prompt.open ? 1 : 0.94

        Behavior on opacity {
            NumberAnimation { duration: 180; easing.type: Easing.OutCubic }
        }

        Behavior on scale {
            NumberAnimation { duration: 180; easing.type: Easing.OutCubic }
        }

        ColumnLayout {
            id: layout

            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: parent.top
            anchors.margins: 22
            spacing: 16

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 3

                Text {
                    Layout.fillWidth: true
                    text: Prompt.title
                    color: Colors.textBright
                    font.pixelSize: 15
                    font.bold: true
                    elide: Text.ElideRight
                }

                Text {
                    Layout.fillWidth: true
                    visible: Prompt.subtitle.length > 0
                    text: Prompt.subtitle
                    color: Colors.textMuted
                    font.pixelSize: 11
                    elide: Text.ElideRight
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 44
                radius: 12
                color: Colors.surface
                border.color: input.activeFocus ? Colors.blue : Colors.border
                border.width: 1

                Behavior on border.color {
                    ColorAnimation { duration: 150 }
                }

                TextInput {
                    id: input

                    anchors.fill: parent
                    anchors.leftMargin: 16
                    anchors.rightMargin: 16
                    echoMode: TextInput.Password
                    color: Colors.textBright
                    font.pixelSize: 12
                    selectByMouse: true
                    selectionColor: Colors.blue
                    verticalAlignment: TextInput.AlignVCenter
                    onAccepted: if (text.length > 0) Prompt.submit(text)
                    Keys.onEscapePressed: Prompt.close()

                    Text {
                        anchors.fill: parent
                        visible: input.text.length === 0
                        text: Prompt.placeholder
                        color: Colors.textMuted
                        font: input.font
                        verticalAlignment: Text.AlignVCenter
                        elide: Text.ElideRight
                    }
                }
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 10

                Item { Layout.fillWidth: true }

                Rectangle {
                    implicitWidth: cancelText.width + 28
                    implicitHeight: 36
                    radius: height / 2
                    color: cancelHover.hovered ? Colors.surface : "transparent"

                    Behavior on color {
                        ColorAnimation { duration: 150 }
                    }

                    Text {
                        id: cancelText
                        anchors.centerIn: parent
                        text: "Cancel"
                        color: Colors.text
                        font.pixelSize: 12
                    }

                    HoverHandler {
                        id: cancelHover
                        cursorShape: Qt.PointingHandCursor
                    }

                    TapHandler {
                        onTapped: Prompt.close()
                    }
                }

                Rectangle {
                    implicitWidth: actionText.width + 36
                    implicitHeight: 36
                    radius: height / 2
                    color: input.text.length === 0 ? Colors.surface
                        : actionHover.hovered ? Qt.lighter(Colors.blue, 1.1) : Colors.blue

                    Behavior on color {
                        ColorAnimation { duration: 150 }
                    }

                    Text {
                        id: actionText
                        anchors.centerIn: parent
                        text: Prompt.action
                        color: input.text.length === 0 ? Colors.textMuted : Colors.background
                        font.pixelSize: 12
                        font.bold: true
                    }

                    HoverHandler {
                        id: actionHover
                        cursorShape: Qt.PointingHandCursor
                    }

                    TapHandler {
                        enabled: input.text.length > 0
                        onTapped: Prompt.submit(input.text)
                    }
                }
            }
        }
    }
}
