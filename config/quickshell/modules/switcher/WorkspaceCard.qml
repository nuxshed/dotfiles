pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import Quickshell.Widgets
import "../../config"
import "../../services"

Rectangle {
    id: root

    required property var workspace
    required property bool selected

    readonly property var monitor: HyprlandData.monitors.find(m => m.id === workspace.monitorID) ?? HyprlandData.monitors[0]
    readonly property real mw: (monitor?.width ?? 1920) / (monitor?.scale ?? 1)
    readonly property real mh: (monitor?.height ?? 1080) / (monitor?.scale ?? 1)
    readonly property real previewWidth: 260
    readonly property real scale: previewWidth / mw
    readonly property var windows: HyprlandData.windowList.filter(w => w.workspace.id === workspace.id && w.mapped && !w.hidden)

    signal clicked
    signal dragStarted(string address, string icon)
    signal dragMoved(point pos)
    signal dropped(string address, point pos)

    implicitWidth: previewWidth + 16
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
            Layout.preferredWidth: root.previewWidth
            Layout.preferredHeight: Math.round(root.mh * root.scale)
            radius: 8
            color: Colors.surface

            Repeater {
                model: root.windows

                Item {
                    id: win

                    required property var modelData

                    readonly property string icon: Switcher.icon(modelData.class)
                    readonly property real homeX: (modelData.at[0] - (root.monitor?.x ?? 0)) * root.scale
                    readonly property real homeY: (modelData.at[1] - (root.monitor?.y ?? 0)) * root.scale

                    x: homeX
                    y: homeY
                    z: modelData.floating ? 1 : 0
                    opacity: grab.dragging ? 0.3 : 1
                    width: Math.max(8, modelData.size[0] * root.scale)
                    height: Math.max(8, modelData.size[1] * root.scale)

                    ClippingRectangle {
                        anchors.fill: parent
                        radius: 4
                        color: Colors.subtle
                        border.color: Colors.outline
                        border.width: 1

                        ScreencopyView {
                            id: view
                            anchors.fill: parent
                            captureSource: Switcher.shown ? Switcher.toplevel(win.modelData.address) : null
                            live: true
                        }

                        IconImage {
                            anchors.centerIn: parent
                            implicitSize: Math.min(28, parent.width * 0.5, parent.height * 0.5)
                            visible: !view.hasContent && win.icon !== ""
                            source: win.icon ? Quickshell.iconPath(win.icon, true) : ""
                        }
                    }

                    MouseArea {
                        id: grab

                        property point origin
                        property bool dragging: false

                        anchors.fill: parent
                        cursorShape: dragging ? Qt.ClosedHandCursor : Qt.PointingHandCursor
                        onPressed: event => {
                            origin = Qt.point(event.x, event.y);
                            dragging = false;
                        }
                        onPositionChanged: event => {
                            if (!dragging && Math.hypot(event.x - origin.x, event.y - origin.y) > 6) {
                                dragging = true;
                                root.dragStarted(win.modelData.address, win.icon);
                            }
                            if (dragging)
                                root.dragMoved(mapToItem(null, event.x, event.y));
                        }
                        onReleased: event => {
                            if (dragging)
                                root.dropped(win.modelData.address, mapToItem(null, event.x, event.y));
                            else
                                root.clicked();
                            dragging = false;
                        }
                    }
                }
            }
        }

        RowLayout {
            Layout.preferredWidth: root.previewWidth
            spacing: 6

            Text {
                text: root.workspace?.name ?? ""
                color: root.selected ? Colors.textBright : Colors.text
                font.pixelSize: 11
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            Text {
                Layout.fillWidth: true
                text: root.windows.length === 1 ? "1 window" : root.windows.length + " windows"
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
                horizontalAlignment: Text.AlignRight
            }
        }
    }

    HoverHandler {
        id: hover
        cursorShape: Qt.PointingHandCursor
    }

    MouseArea {
        anchors.fill: parent
        z: -1
        onClicked: root.clicked()
    }
}
