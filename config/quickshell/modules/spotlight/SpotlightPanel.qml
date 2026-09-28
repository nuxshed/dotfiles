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
    readonly property int listMax: 440

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

        MouseArea {
            anchors.fill: parent
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

                    Keys.onEscapePressed: Spotlight.back()
                    Keys.onUpPressed: Spotlight.vertical(-1)
                    Keys.onDownPressed: Spotlight.vertical(1)
                    Keys.onLeftPressed: (event) => {
                        if (Spotlight.home)
                            Spotlight.move(-1)
                        else
                            event.accepted = false
                    }
                    Keys.onRightPressed: (event) => {
                        if (Spotlight.home)
                            Spotlight.move(1)
                        else
                            event.accepted = false
                    }
                    Keys.onTabPressed: Spotlight.complete()
                    Keys.onReturnPressed: (event) => Spotlight.activate(event.modifiers & Qt.AltModifier)
                    Keys.onEnterPressed: (event) => Spotlight.activate(event.modifiers & Qt.AltModifier)
                    Keys.onPressed: (event) => {
                        if (!(event.modifiers & Qt.ControlModifier))
                            return
                        if (event.key === Qt.Key_Space) {
                            Spotlight.toggleActions()
                            event.accepted = true
                        } else if (event.key === Qt.Key_J) {
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
                visible: Spotlight.home || list.count > 0
                color: Colors.border
            }

            SpotlightHome {
                Layout.fillWidth: true
                Layout.margins: 12
                visible: Spotlight.home
            }

            ListView {
                id: list

                Layout.fillWidth: true
                Layout.preferredHeight: visible ? Math.min(root.listMax, contentHeight) : 0
                Layout.topMargin: visible && count > 0 ? 6 : 0
                Layout.bottomMargin: visible && count > 0 ? 6 : 0
                visible: !Spotlight.home
                clip: true
                model: Spotlight.results
                currentIndex: Spotlight.selected
                highlightMoveDuration: 0
                boundsBehavior: Flickable.StopAtBounds

                onCurrentIndexChanged: positionViewAtIndex(currentIndex, ListView.Contain)

                Connections {
                    target: Spotlight

                    function onExpandedChanged() {
                        reveal.restart()
                    }
                }

                Timer {
                    id: reveal
                    interval: 240
                    onTriggered: list.positionViewAtIndex(list.currentIndex, ListView.Contain)
                }

                delegate: ResultRow {
                    required property int index
                    required property var modelData

                    width: list.width
                    item: modelData ?? ({})
                    selected: index === Spotlight.selected
                    showSection: index === 0 || (Spotlight.results[index - 1]?.section ?? "") !== (modelData?.section ?? "")
                    expanded: selected && Spotlight.expanded
                    actions: selected ? Spotlight.currentActions : []
                    action: Spotlight.action

                    onClicked: {
                        Spotlight.selected = index
                        Spotlight.activate(false)
                    }

                    onAltClicked: {
                        Spotlight.selected = index
                        Spotlight.activate(true)
                    }

                    onActionClicked: (i) => {
                        Spotlight.action = i
                        Spotlight.activate(false)
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
