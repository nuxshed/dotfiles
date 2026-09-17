import QtQuick
import "../../config"
import "../../services"

Item {
    id: root

    readonly property real unit: height / 768
    readonly property bool typing: Lock.buffer.length > 0 || Lock.busy || Lock.error.length > 0
    readonly property string caption: {
        if (Lock.busy)
            return "CHECKING";
        if (Lock.error.length > 0)
            return Lock.error.toUpperCase();
        return "ENTER PASSWORD";
    }

    property bool ready: false
    property real shake: 0

    focus: true
    opacity: ready && !Lock.dimmed && !Lock.unlocking ? 1 : 0

    Behavior on opacity {
        NumberAnimation {
            duration: Lock.dimmed ? 1000 : 300
            easing.type: Easing.InOutQuad
        }
    }

    Component.onCompleted: ready = true

    Keys.onPressed: event => {
        event.accepted = true;

        const wake = Lock.dimmed;
        Lock.touch();
        if (wake)
            return;

        if (event.key === Qt.Key_Tab)
            Lock.toggleMode();
        else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter)
            Lock.submit();
        else if (event.key === Qt.Key_Backspace)
            Lock.erase(event.modifiers & Qt.ControlModifier);
        else if (event.key === Qt.Key_Escape)
            Lock.erase(true);
        else if (event.text.length > 0 && event.text.charCodeAt(0) >= 0x20)
            Lock.type(event.text);
    }

    Connections {
        target: Lock

        function onErrorChanged(): void {
            if (Lock.error.length > 0)
                shakeAnim.restart();
        }
    }

    SequentialAnimation {
        id: shakeAnim

        NumberAnimation { target: root; property: "shake"; to: -10 * root.unit; duration: 50 }
        NumberAnimation { target: root; property: "shake"; to: 10 * root.unit; duration: 50 }
        NumberAnimation { target: root; property: "shake"; to: -6 * root.unit; duration: 50 }
        NumberAnimation { target: root; property: "shake"; to: 6 * root.unit; duration: 50 }
        NumberAnimation { target: root; property: "shake"; to: 0; duration: 50 }
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.AllButtons
        hoverEnabled: true
        cursorShape: Qt.BlankCursor
        onPositionChanged: Lock.touch()
        onPressed: Lock.touch()
        onWheel: wheel => wheel.accepted = true
    }

    Loader {
        anchors.fill: parent
        sourceComponent: Lock.mode === "bounce" ? bounce : clock
    }

    Component {
        id: clock

        OrbitalClock {
            unit: root.unit
            active: !Lock.dimmed
        }
    }

    Component {
        id: bounce

        BounceClock {
            unit: root.unit
        }
    }

    Item {
        anchors.right: parent.right
        anchors.rightMargin: 80 * root.unit
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 80 * root.unit
        width: 360 * root.unit
        height: dots.height + captionText.height + 14 * root.unit
        opacity: Lock.mode === "clock" || root.typing ? 1 : 0
        transform: Translate { x: root.shake }

        Behavior on opacity {
            NumberAnimation { duration: 300 }
        }

        Row {
            id: dots

            anchors.right: parent.right
            anchors.top: parent.top
            height: 8 * root.unit
            spacing: 8 * root.unit
            opacity: Lock.busy ? 0.4 : 1

            Behavior on opacity {
                NumberAnimation { duration: 200 }
            }

            Repeater {
                model: Math.min(Lock.buffer.length, 32)

                Rectangle {
                    width: 8 * root.unit
                    height: width
                    radius: width / 2
                    color: Colors.textBright
                    scale: 0

                    Component.onCompleted: scale = 1

                    Behavior on scale {
                        NumberAnimation { duration: 150; easing.type: Easing.OutBack }
                    }
                }
            }
        }

        Text {
            id: captionText

            anchors.right: parent.right
            anchors.bottom: parent.bottom
            text: root.caption
            font.pixelSize: 11 * root.unit
            font.family: Fonts.family
            font.letterSpacing: 4 * root.unit
            color: Lock.error.length > 0 ? Colors.red : Colors.textMuted

            Behavior on color {
                ColorAnimation { duration: 150 }
            }
        }
    }
}
