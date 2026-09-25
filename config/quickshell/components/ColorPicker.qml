pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../config"

ColumnLayout {
    id: root

    property string current: "#a8ccff"
    property var colors: []
    property real hue: 0
    property real sat: 0
    property real val: 1
    property bool expanded: false

    readonly property bool custom: !root.colors.some(c => Qt.colorEqual(c, root.current))

    readonly property color value: Qt.hsva(hue, sat, val, 1)
    readonly property string hex: value.toString()

    signal picked(string color)

    function load(c: string): void {
        if (c.length === 0 || Qt.colorEqual(c, root.value))
            return;
        const q = Qt.color(c);
        if (q.hsvHue >= 0)
            root.hue = q.hsvHue;
        root.sat = q.hsvSaturation;
        root.val = q.hsvValue;
    }

    function valid(t: string): bool {
        return /^#?[0-9a-fA-F]{6}$/.test(t);
    }

    onCurrentChanged: load(current)
    Component.onCompleted: load(current)

    spacing: 10

    Flow {
        Layout.fillWidth: true
        spacing: 6

        Repeater {
            model: root.colors

            Rectangle {
                id: swatch

                required property string modelData

                width: 18
                height: 18
                radius: 9
                color: modelData
                border.width: 2
                border.color: Qt.colorEqual(modelData, root.current) ? Colors.textBright : "transparent"
                scale: swatchArea.containsMouse ? 1.12 : 1

                Behavior on scale {
                    NumberAnimation { duration: 120; easing.type: Easing.OutCubic }
                }

                MouseArea {
                    id: swatchArea
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: {
                        root.load(swatch.modelData);
                        root.picked(root.hex);
                    }
                }
            }
        }

        Rectangle {
            width: 18
            height: 18
            radius: 9
            border.width: 2
            border.color: root.expanded || root.custom ? Colors.textBright : "transparent"
            scale: customArea.containsMouse ? 1.12 : 1
            gradient: Gradient {
                orientation: Gradient.Horizontal
                GradientStop { position: 0; color: "#ff5f6d" }
                GradientStop { position: 0.35; color: "#ffd86f" }
                GradientStop { position: 0.65; color: "#5fd4a8" }
                GradientStop { position: 1; color: "#7a8cff" }
            }

            Behavior on scale {
                NumberAnimation { duration: 120; easing.type: Easing.OutCubic }
            }

            MouseArea {
                id: customArea
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: root.expanded = !root.expanded
            }
        }
    }

    Item {
        visible: root.expanded
        Layout.fillWidth: true
        implicitHeight: 130

        Rectangle {
            anchors.fill: parent
            radius: 8
            color: Qt.hsva(root.hue, 1, 1, 1)
        }

        Rectangle {
            anchors.fill: parent
            radius: 8
            gradient: Gradient {
                orientation: Gradient.Horizontal
                GradientStop { position: 0; color: "#ffffffff" }
                GradientStop { position: 1; color: "#00ffffff" }
            }
        }

        Rectangle {
            anchors.fill: parent
            radius: 8
            gradient: Gradient {
                GradientStop { position: 0; color: "#00000000" }
                GradientStop { position: 1; color: "#ff000000" }
            }
        }

        Thumb {
            x: root.sat * parent.width - width / 2
            y: (1 - root.val) * parent.height - height / 2
        }

        MouseArea {
            anchors.fill: parent
            preventStealing: true
            cursorShape: Qt.CrossCursor

            function set(mouse: var): void {
                root.sat = Math.max(0, Math.min(1, mouse.x / width));
                root.val = 1 - Math.max(0, Math.min(1, mouse.y / height));
            }

            onPressed: mouse => set(mouse)
            onPositionChanged: mouse => set(mouse)
            onReleased: root.picked(root.hex)
        }
    }

    Item {
        visible: root.expanded
        Layout.fillWidth: true
        implicitHeight: 14

        Rectangle {
            anchors.fill: parent
            radius: height / 2
            gradient: Gradient {
                orientation: Gradient.Horizontal
                GradientStop { position: 0 / 6; color: "#ff0000" }
                GradientStop { position: 1 / 6; color: "#ffff00" }
                GradientStop { position: 2 / 6; color: "#00ff00" }
                GradientStop { position: 3 / 6; color: "#00ffff" }
                GradientStop { position: 4 / 6; color: "#0000ff" }
                GradientStop { position: 5 / 6; color: "#ff00ff" }
                GradientStop { position: 6 / 6; color: "#ff0000" }
            }
        }

        Thumb {
            x: root.hue * parent.width - width / 2
            anchors.verticalCenter: parent.verticalCenter
            fill: Qt.hsva(root.hue, 1, 1, 1)
        }

        MouseArea {
            anchors.fill: parent
            anchors.margins: -4
            preventStealing: true
            cursorShape: Qt.PointingHandCursor

            function set(mouse: var): void {
                root.hue = Math.max(0, Math.min(0.9999, (mouse.x - 4) / (width - 8)));
            }

            onPressed: mouse => set(mouse)
            onPositionChanged: mouse => set(mouse)
            onReleased: root.picked(root.hex)
        }
    }

    Rectangle {
        visible: root.expanded
        Layout.fillWidth: true
        implicitHeight: 30
        radius: 8
        color: Colors.background
        border.width: 1
        border.color: hexInput.activeFocus ? Colors.outline : "transparent"

        Behavior on border.color {
            ColorAnimation { duration: 150 }
        }

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 8
            anchors.rightMargin: 8
            spacing: 8

            Rectangle {
                implicitWidth: 14
                implicitHeight: 14
                radius: 7
                color: root.value
            }

            Text {
                text: "#"
                color: Colors.textMuted
                font.pixelSize: 12
                font.family: Fonts.family
            }

            TextInput {
                id: hexInput

                Layout.fillWidth: true
                text: root.hex.slice(1)
                color: Colors.text
                selectionColor: Colors.primaryContainer
                selectedTextColor: Colors.textBright
                font.pixelSize: 12
                font.family: Fonts.family
                maximumLength: 6
                selectByMouse: true
                validator: RegularExpressionValidator { regularExpression: /[0-9a-fA-F]{0,6}/ }
                onAccepted: if (root.valid(text)) {
                    root.load("#" + text);
                    root.picked(root.hex);
                }
                onActiveFocusChanged: if (!activeFocus)
                    text = Qt.binding(() => root.hex.slice(1))
            }
        }
    }

    component Thumb: Rectangle {
        property color fill: root.value

        width: 16
        height: 16
        radius: 8
        color: fill
        border.width: 2
        border.color: "white"
    }
}
