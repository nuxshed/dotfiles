import QtQuick
import Quickshell
import "../../config"
import "../../services"

Item {
    id: root

    readonly property real unit: height / 768
    readonly property string status: {
        if (Lock.busy)
            return "CHECKING";
        if (Lock.error.length > 0)
            return Lock.error.toUpperCase();
        if (Lock.buffer.length > 0)
            return "•".repeat(Math.min(Lock.buffer.length, 24));
        return "WAITING FOR KEY";
    }

    focus: true

    Keys.onPressed: event => {
        event.accepted = true;

        if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter)
            Lock.submit();
        else if (event.key === Qt.Key_Backspace)
            Lock.erase(event.modifiers & Qt.ControlModifier);
        else if (event.key === Qt.Key_Escape)
            Lock.erase(true);
        else if (event.text.length > 0 && event.text.charCodeAt(0) >= 0x20)
            Lock.type(event.text);
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.AllButtons
        cursorShape: Qt.ArrowCursor
        onWheel: wheel => wheel.accepted = true
    }

    OrbitalClock {
        anchors.fill: parent
        unit: root.unit
    }

    Column {
        anchors.right: parent.right
        anchors.rightMargin: 80 * root.unit
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 80 * root.unit
        width: 360 * root.unit
        spacing: 10 * root.unit

        Text {
            width: parent.width
            horizontalAlignment: Text.AlignRight
            text: (Quickshell.env("USER") ?? "").toUpperCase()
            font.pixelSize: 18 * root.unit
            font.letterSpacing: 8 * root.unit
            font.bold: true
            color: Colors.textBright
        }

        Text {
            width: parent.width
            horizontalAlignment: Text.AlignRight
            text: root.status
            font.pixelSize: 11 * root.unit
            font.letterSpacing: 4 * root.unit
            color: Lock.error.length > 0 ? Colors.red : Colors.textMuted

            Behavior on color {
                ColorAnimation { duration: 150 }
            }
        }
    }
}
