import Quickshell
import Quickshell.Wayland
import QtQuick
import "../../services"

Variants {
    model: Quickshell.screens

    PanelWindow {
        required property var modelData

        screen: modelData
        visible: Capture.shielding
        exclusionMode: ExclusionMode.Ignore
        color: "transparent"

        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.namespace: "qs:cursor-shield"
        WlrLayershell.keyboardFocus: visible ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

        anchors {
            top: true
            bottom: true
            left: true
            right: true
        }

        MouseArea {
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.BlankCursor
            onEntered: Capture.shieldEntered()
        }
    }
}
