pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import "../../components"
import "../../config"
import "../../services"

Item {
    id: root

    readonly property var info: Files.openWith
    readonly property bool shown: info !== null
    property bool always: false
    property int cursor: 0

    readonly property var apps: {
        if (!root.info)
            return [];
        const all = DesktopEntries.applications.values;
        const ids = root.info.apps.map(a => a.id.replace(/\.desktop$/, ""));
        const q = filter.text.trim().toLowerCase();
        const match = e => q.length === 0 || e.name.toLowerCase().includes(q) || (e.genericName ?? "").toLowerCase().includes(q);
        const rec = root.info.apps.filter(match).map((a, i) => ({ entry: a, section: "Recommended", hint: i === 0 ? "Default" : "" }));
        const other = all.filter(e => !e.noDisplay && !ids.includes(e.id.replace(/\.desktop$/, "")) && match(e));
        other.sort((a, b) => a.name.localeCompare(b.name));
        return rec.concat(other.map(e => ({ entry: e, section: "Other applications", hint: e.genericName ?? "" })));
    }

    signal closed

    visible: opacity > 0
    opacity: root.shown ? 1 : 0
    z: 30

    Behavior on opacity {
        Anim { duration: 140 }
    }

    onShownChanged: {
        if (root.shown) {
            filter.text = "";
            root.always = false;
            root.cursor = 0;
            filter.input.forceActiveFocus();
        } else {
            root.closed();
        }
    }

    onAppsChanged: root.cursor = 0

    function launchCursor(): void {
        if (root.cursor >= 0 && root.cursor < root.apps.length)
            Files.launchWith(root.apps[root.cursor].entry, root.always);
    }

    Rectangle {
        anchors.fill: parent
        color: "#000000"
        opacity: 0.4

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.AllButtons
            onClicked: Files.openWith = null
        }
    }

    Rectangle {
        anchors.centerIn: parent
        width: 440
        height: 480
        radius: 16
        color: Colors.background
        border.width: 1
        border.color: Colors.border
        scale: root.shown ? 1 : 0.96
        clip: true

        Behavior on scale {
            Anim { duration: 140 }
        }

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.AllButtons
        }

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 20
            spacing: 12

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 2

                Text {
                    Layout.fillWidth: true
                    text: "Open with"
                    color: Colors.textBright
                    font.pixelSize: 14
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }

                Text {
                    Layout.fillWidth: true
                    text: root.info ? `${root.info.name}${root.info.mime.length > 0 ? " · " + root.info.mime : ""}` : ""
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                    elide: Text.ElideMiddle
                }
            }

            Field {
                id: filter

                Layout.fillWidth: true
                icon: "search"
                placeholder: "Search applications"
                onAccepted: root.launchCursor()
                onEscaped: Files.openWith = null
                onDown: root.cursor = Math.min(root.apps.length - 1, root.cursor + 1)

                input.Keys.onUpPressed: root.cursor = Math.max(0, root.cursor - 1)
            }

            ListView {
                id: list

                Layout.fillWidth: true
                Layout.fillHeight: true
                clip: true
                model: root.apps
                spacing: 1
                boundsBehavior: Flickable.StopAtBounds
                currentIndex: root.cursor
                onCurrentIndexChanged: positionViewAtIndex(currentIndex, ListView.Contain)

                WheelHandler {
                    onWheel: event => {
                        const step = event.pixelDelta.y !== 0 ? event.pixelDelta.y * 3 : event.angleDelta.y / 120 * 150;
                        list.contentY = Math.max(0, Math.min(Math.max(0, list.contentHeight - list.height), list.contentY - step));
                    }
                }

                delegate: Item {
                    id: row

                    required property int index
                    required property var modelData

                    readonly property bool header: index === 0 || root.apps[index - 1].section !== modelData.section

                    width: list.width
                    height: (header ? 24 : 0) + 36

                    Text {
                        visible: row.header
                        x: 10
                        height: 24
                        text: row.modelData.section
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
                        height: 34
                        radius: 8
                        color: root.cursor === row.index ? Colors.surfaceActive : rowMouse.containsMouse ? Colors.surface : "transparent"

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: 10
                            anchors.rightMargin: 10
                            spacing: 10

                            IconImage {
                                Layout.preferredWidth: 20
                                Layout.preferredHeight: 20
                                source: Quickshell.iconPath(row.modelData.entry.icon, true)
                            }

                            Text {
                                Layout.fillWidth: true
                                text: row.modelData.entry.name
                                color: root.cursor === row.index ? Colors.textBright : Colors.text
                                font.pixelSize: 12
                                font.family: Fonts.family
                                elide: Text.ElideRight
                            }

                            Text {
                                text: row.modelData.hint
                                color: Colors.textMuted
                                font.pixelSize: 10
                                font.family: Fonts.family
                                elide: Text.ElideRight
                            }
                        }

                        MouseArea {
                            id: rowMouse
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: Files.launchWith(row.modelData.entry, root.always)
                        }
                    }
                }

                Text {
                    anchors.centerIn: parent
                    visible: root.apps.length === 0
                    text: root.info && !root.info.ready ? "Looking up applications…" : "No applications"
                    color: Colors.textMuted
                    font.pixelSize: 12
                    font.family: Fonts.family
                }
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 10

                Toggle {
                    checked: root.always
                    onToggled: root.always = !root.always
                }

                Text {
                    Layout.fillWidth: true
                    text: "Always open this type with the chosen app"
                    color: Colors.textDimmed
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                TextButton {
                    text: "Cancel"
                    onClicked: Files.openWith = null
                }
            }
        }
    }
}
