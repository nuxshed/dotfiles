import QtQuick
import Quickshell
import Quickshell.Wayland
import "../../services"

PanelWindow {
    id: root

    visible: Files.open
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:files"
    WlrLayershell.keyboardFocus: Files.open ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    mask: view.dragging ? cardMask : null

    Region {
        id: cardMask
        item: view.cardItem
    }

    FilesView {
        id: view
        anchors.fill: parent
        shown: Files.open
    }
}
