pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import "../../components"
import "../../config"
import "../../services"
import "../files"
import "../preview"

FloatingWindow {
    id: root

    visible: SysMon.maintOpen
    implicitWidth: 600
    implicitHeight: 620
    minimumSize.width: 480
    minimumSize.height: 480
    color: "transparent"
    title: "Maintenance"

    WindowChrome { titleHeight: 64 }

    onVisibleChanged: if (visible) keyScope.forceActiveFocus()

    Item {
        id: keyScope
        anchors.fill: parent
        focus: true
        Keys.onEscapePressed: SysMon.maintOpen = false
    }

    Confirm { id: confirm; anchors.fill: parent; z: 50 }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 20
        spacing: 14

        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: false

            Column {
                Layout.fillWidth: true
                spacing: 3

                Text {
                    text: "Maintenance"
                    color: Colors.textBright
                    font.pixelSize: 16
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }
                Text {
                    text: "Reclaim space from the trash and the Nix store"
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }
            }
            PreviewButton {
                implicitWidth: 28
                implicitHeight: 28
                icon: "close"
                onClicked: SysMon.maintOpen = false
            }
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: false
            implicitHeight: 84
            radius: 10
            color: Colors.surfaceActive
            border.width: 1
            border.color: Colors.outline

            RowLayout {
                anchors.fill: parent
                anchors.margins: 18
                spacing: 16

                MaterialIcon {
                    text: "delete"
                    size: 26
                    color: Colors.textDimmed
                }
                Column {
                    Layout.fillWidth: true
                    spacing: 3

                    Text {
                        text: SysMon.trashCount > 0 ? SysMon.fmtBytes(SysMon.trashSize, 1) : "Trash is empty"
                        color: Colors.textBright
                        font.pixelSize: 18
                        font.family: Fonts.family
                        font.weight: Font.Medium
                    }
                    Text {
                        text: SysMon.trashCount > 0 ? `${SysMon.trashCount} item${SysMon.trashCount === 1 ? "" : "s"} in the trash` : "Nothing to reclaim"
                        color: Colors.textMuted
                        font.pixelSize: 11
                        font.family: Fonts.family
                    }
                }
                TextButton {
                    text: SysMon.maintenanceBusy ? "Emptying…" : "Empty trash"
                    primary: true
                    danger: true
                    implicitHeight: 32
                    enabled: SysMon.trashCount > 0 && !SysMon.maintenanceBusy
                    onClicked: confirm.ask("Empty the trash?", `${SysMon.fmtBytes(SysMon.trashSize, 1)} in ${SysMon.trashCount} items will be deleted permanently.`, "Empty", () => SysMon.emptyTrash())
                }
            }
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            radius: 10
            color: Colors.surfaceActive
            border.width: 1
            border.color: Colors.outline

            ColumnLayout {
                anchors.fill: parent
                anchors.margins: 18
                spacing: 12

                RowLayout {
                    Layout.fillWidth: true
                    Layout.fillHeight: false
                    spacing: 16

                    MaterialIcon {
                        text: "layers"
                        size: 26
                        color: Colors.textDimmed
                    }
                    Column {
                        Layout.fillWidth: true
                        spacing: 3

                        Text {
                            text: `${SysMon.generations.length} system generation${SysMon.generations.length === 1 ? "" : "s"}`
                            color: Colors.textBright
                            font.pixelSize: 18
                            font.family: Fonts.family
                            font.weight: Font.Medium
                        }
                        Text {
                            text: `Current closure ${SysMon.fmtBytes(SysMon.nixClosure, 1)} · every kept generation pins its own closure`
                            color: Colors.textMuted
                            font.pixelSize: 11
                            font.family: Fonts.family
                        }
                    }
                }

                Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: Colors.outline }

                ListView {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    clip: true
                    model: SysMon.generations
                    boundsBehavior: Flickable.StopAtBounds
                    spacing: 2

                    delegate: Item {
                        id: gen
                        required property var modelData
                        width: ListView.view.width
                        height: 28

                        RowLayout {
                            anchors.fill: parent
                            spacing: 12

                            Text {
                                Layout.preferredWidth: 70
                                text: `#${gen.modelData.n}`
                                color: gen.modelData.current ? Colors.textBright : Colors.text
                                font.pixelSize: 12
                                font.family: Fonts.family
                                font.weight: gen.modelData.current ? Font.Medium : Font.Normal
                            }
                            Text {
                                Layout.fillWidth: true
                                text: new Date(gen.modelData.time).toLocaleString(Qt.locale(), "ddd d MMM yyyy, HH:mm")
                                color: Colors.textDimmed
                                font.pixelSize: 11
                                font.family: Fonts.family
                            }
                            Rectangle {
                                visible: gen.modelData.current
                                width: curText.width + 12
                                height: 18
                                radius: 9
                                color: Colors.subtle

                                Text {
                                    id: curText
                                    anchors.centerIn: parent
                                    text: "current"
                                    color: Colors.textBright
                                    font.pixelSize: 10
                                    font.family: Fonts.family
                                }
                            }
                        }
                    }
                }

                Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: Colors.outline }

                RowLayout {
                    Layout.fillWidth: true
                    Layout.fillHeight: false
                    spacing: 6

                    Text {
                        Layout.fillWidth: true
                        text: "Runs in a terminal"
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                        elide: Text.ElideRight
                    }
                    TextButton {
                        text: "Optimise store"
                        implicitHeight: 30
                        onClicked: SysMon.runInTerminal("pls optimise")
                    }
                    TextButton {
                        text: "Delete old generations"
                        implicitHeight: 30
                        enabled: SysMon.generations.length > 1
                        onClicked: confirm.ask("Delete old system generations?", "Runs `pls gc all`, which removes every generation except the current one and collects garbage. You will not be able to boot into older configurations.", "Delete", () => SysMon.runInTerminal("pls gc all"))
                    }
                    TextButton {
                        text: "Collect garbage"
                        primary: true
                        implicitHeight: 30
                        onClicked: SysMon.runInTerminal("pls gc")
                    }
                }
            }
        }
    }
}
