pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import "../../components"
import "../../config"
import "../../services/spotlight"

PanelWindow {
    id: root

    readonly property int cardWidth: 620
    readonly property int listMax: 380

    visible: Spotlight.open
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:spotlight"
    WlrLayershell.keyboardFocus: Spotlight.open ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    onVisibleChanged: {
        if (visible)
            input.forceActiveFocus()
    }

    Connections {
        target: Spotlight

        function onQueryReset() {
            input.text = Spotlight.query
            input.cursorPosition = input.text.length
        }
    }

    Rectangle {
        anchors.fill: parent
        color: "#000000"
        opacity: Spotlight.open ? 0.45 : 0

        Behavior on opacity {
            NumberAnimation { duration: 150 }
        }

        TapHandler {
            onTapped: Spotlight.hide()
        }
    }

    Rectangle {
        id: card

        anchors.horizontalCenter: parent.horizontalCenter
        y: Math.round(parent.height * 0.22)
        width: root.cardWidth
        height: layout.implicitHeight
        radius: 12
        color: Colors.background
        border.color: Colors.border
        border.width: 1
        clip: true
        opacity: Spotlight.open ? 1 : 0
        scale: Spotlight.open ? 1 : 0.97

        Behavior on opacity {
            Anim { duration: 160 }
        }

        Behavior on scale {
            Anim { duration: 160 }
        }

        Behavior on height {
            Anim { duration: 180 }
        }

        ColumnLayout {
            id: layout

            width: parent.width
            spacing: 0

            RowLayout {
                Layout.fillWidth: true
                Layout.preferredHeight: 52
                Layout.leftMargin: 16
                Layout.rightMargin: 16
                spacing: 10

                MaterialIcon {
                    text: "search"
                    size: 20
                    color: Colors.textDimmed
                }

                Rectangle {
                    visible: scopeLabel.text.length > 0
                    Layout.preferredWidth: scopeLabel.width + 16
                    Layout.preferredHeight: 22
                    radius: 6
                    color: Colors.surfaceActive

                    Text {
                        id: scopeLabel
                        anchors.centerIn: parent
                        text: Spotlight.scope?.label ?? ""
                        color: Colors.blue
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }
                }

                TextInput {
                    id: input

                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    color: Colors.textBright
                    font.pixelSize: 14
                    font.family: Fonts.family
                    selectByMouse: true
                    selectionColor: Colors.blue
                    verticalAlignment: TextInput.AlignVCenter
                    onTextChanged: Spotlight.query = text

                    Keys.onEscapePressed: Spotlight.hide()
                    Keys.onUpPressed: Spotlight.move(-1)
                    Keys.onDownPressed: Spotlight.move(1)
                    Keys.onTabPressed: Spotlight.complete()
                    Keys.onReturnPressed: (event) => Spotlight.activate(event.modifiers & Qt.AltModifier)
                    Keys.onEnterPressed: (event) => Spotlight.activate(event.modifiers & Qt.AltModifier)
                    Keys.onPressed: (event) => {
                        if (!(event.modifiers & Qt.ControlModifier))
                            return
                        if (event.key === Qt.Key_J) {
                            Spotlight.move(1)
                            event.accepted = true
                        } else if (event.key === Qt.Key_K) {
                            Spotlight.move(-1)
                            event.accepted = true
                        }
                    }

                    Text {
                        anchors.fill: parent
                        visible: input.text.length === 0
                        text: "Search apps, files, the web…"
                        color: Colors.textMuted
                        font: input.font
                        verticalAlignment: Text.AlignVCenter
                    }
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 1
                visible: list.count > 0
                color: Colors.border
            }

            ListView {
                id: list

                Layout.fillWidth: true
                Layout.preferredHeight: Math.min(root.listMax, contentHeight)
                Layout.topMargin: count > 0 ? 6 : 0
                Layout.bottomMargin: count > 0 ? 6 : 0
                clip: true
                model: Spotlight.results
                currentIndex: Spotlight.selected
                highlightMoveDuration: 0
                boundsBehavior: Flickable.StopAtBounds

                onCurrentIndexChanged: positionViewAtIndex(currentIndex, ListView.Contain)

                delegate: ResultRow {
                    required property int index
                    required property var modelData

                    width: list.width
                    item: modelData ?? ({})
                    selected: index === Spotlight.selected
                    showSection: index === 0 || (Spotlight.results[index - 1]?.section ?? "") !== (modelData?.section ?? "")

                    onClicked: {
                        Spotlight.selected = index
                        Spotlight.activate(false)
                    }

                    onAltClicked: {
                        Spotlight.selected = index
                        Spotlight.activate(true)
                    }
                }
            }

            Item {
                Layout.fillWidth: true
                Layout.preferredHeight: 44
                visible: list.count === 0 && input.text.length > 0

                Text {
                    anchors.centerIn: parent
                    text: "No results"
                    color: Colors.textMuted
                    font.pixelSize: 12
                    font.family: Fonts.family
                }
            }
        }
    }
}
