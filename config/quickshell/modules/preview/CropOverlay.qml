import QtQuick
import "../../config"
import "../../services"

Item {
    id: root

    property real paperW: 1
    property real paperH: 1

    // dim everything, punch a hole for the selection
    Item {
        anchors.fill: parent

        Rectangle {
            id: sel
            visible: Preview.cropSel !== null
            x: Preview.cropSel ? Preview.cropSel.x * parent.width : 0
            y: Preview.cropSel ? Preview.cropSel.y * parent.height : 0
            width: Preview.cropSel ? Preview.cropSel.w * parent.width : 0
            height: Preview.cropSel ? Preview.cropSel.h * parent.height : 0
            color: "transparent"
            border.width: 1
            border.color: Colors.blue

            Rectangle { anchors.fill: parent; color: Colors.blue; opacity: 0.12 }

            Repeater {
                model: 4
                Rectangle {
                    required property int index
                    width: 8; height: 8; radius: 4
                    color: Colors.blue
                    x: (index % 2) * (parent.width - width)
                    y: Math.floor(index / 2) * (parent.height - height)
                }
            }
        }

        // four dim panels around the selection
        Repeater {
            model: 4
            Rectangle {
                required property int index
                color: "#000000"
                opacity: 0.55
                x: index === 0 ? 0 : index === 1 ? 0 : index === 2 ? 0 : sel.x + sel.width
                y: index === 0 ? 0 : index === 1 ? sel.y + sel.height : index === 2 ? sel.y : sel.y
                width: index === 0 || index === 1 ? root.width : (index === 2 ? sel.x : root.width - sel.x - sel.width)
                height: index === 0 ? sel.y : index === 1 ? root.height - sel.y - sel.height : sel.height
                visible: Preview.cropSel !== null
            }
        }

        Rectangle {
            anchors.fill: parent
            color: "#000000"
            opacity: 0.35
            visible: Preview.cropSel === null
        }
    }

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.CrossCursor
        preventStealing: true

        property real sx: 0
        property real sy: 0

        function nx(v) { return Math.max(0, Math.min(1, v / width)); }
        function ny(v) { return Math.max(0, Math.min(1, v / height)); }

        onPressed: mouse => {
            sx = nx(mouse.x);
            sy = ny(mouse.y);
            Preview.cropSel = { x: sx, y: sy, w: 0, h: 0 };
        }

        onPositionChanged: mouse => {
            if (!pressed)
                return;
            const cx = nx(mouse.x);
            const cy = ny(mouse.y);
            Preview.cropSel = {
                x: Math.min(sx, cx),
                y: Math.min(sy, cy),
                w: Math.abs(cx - sx),
                h: Math.abs(cy - sy)
            };
        }

        onReleased: {
            const c = Preview.cropSel;
            if (c && (c.w < 0.02 || c.h < 0.02))
                Preview.cropSel = null;
        }
    }
}
