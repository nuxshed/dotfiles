import QtQuick

Item {
    id: root

    property Item target: parent
    property bool centered: false
    property int minWidth: 200
    property int minHeight: 140
    property int grip: 8

    property real sx: 0
    property real sy: 0
    property rect start: Qt.rect(0, 0, 0, 0)
    property real ox: 0
    property real oy: 0

    anchors.fill: parent
    z: 100

    function begin(area, mouse): void {
        const p = area.mapToItem(null, mouse.x, mouse.y);
        sx = p.x;
        sy = p.y;
        start = Qt.rect(target.x, target.y, target.width, target.height);
        ox = target.anchors.horizontalCenterOffset;
        oy = target.anchors.verticalCenterOffset;
    }

    function move(area, mouse, edges): void {
        const p = area.mapToItem(null, mouse.x, mouse.y);
        const dx = p.x - sx;
        const dy = p.y - sy;
        let x = start.x, y = start.y, w = start.width, h = start.height;

        if (edges & Qt.RightEdge)
            w = Math.max(minWidth, start.width + dx);
        if (edges & Qt.BottomEdge)
            h = Math.max(minHeight, start.height + dy);
        if (edges & Qt.LeftEdge) {
            w = Math.max(minWidth, start.width - dx);
            x = start.x + start.width - w;
        }
        if (edges & Qt.TopEdge) {
            h = Math.max(minHeight, start.height - dy);
            y = start.y + start.height - h;
        }

        if (centered) {
            target.anchors.horizontalCenterOffset = ox + (x + w / 2) - (start.x + start.width / 2);
            target.anchors.verticalCenterOffset = oy + (y + h / 2) - (start.y + start.height / 2);
        } else {
            target.x = x;
            target.y = y;
        }
        target.width = w;
        target.height = h;
    }

    Grip { edges: Qt.LeftEdge; x: 0; y: root.grip; width: root.grip; height: parent.height - root.grip * 2; cursorShape: Qt.SizeHorCursor }
    Grip { edges: Qt.RightEdge; x: parent.width - root.grip; y: root.grip; width: root.grip; height: parent.height - root.grip * 2; cursorShape: Qt.SizeHorCursor }
    Grip { edges: Qt.TopEdge; x: root.grip; y: 0; width: parent.width - root.grip * 2; height: root.grip; cursorShape: Qt.SizeVerCursor }
    Grip { edges: Qt.BottomEdge; x: root.grip; y: parent.height - root.grip; width: parent.width - root.grip * 2; height: root.grip; cursorShape: Qt.SizeVerCursor }
    Grip { edges: Qt.LeftEdge | Qt.TopEdge; x: 0; y: 0; width: root.grip; height: root.grip; cursorShape: Qt.SizeFDiagCursor }
    Grip { edges: Qt.RightEdge | Qt.BottomEdge; x: parent.width - root.grip; y: parent.height - root.grip; width: root.grip; height: root.grip; cursorShape: Qt.SizeFDiagCursor }
    Grip { edges: Qt.RightEdge | Qt.TopEdge; x: parent.width - root.grip; y: 0; width: root.grip; height: root.grip; cursorShape: Qt.SizeBDiagCursor }
    Grip { edges: Qt.LeftEdge | Qt.BottomEdge; x: 0; y: parent.height - root.grip; width: root.grip; height: root.grip; cursorShape: Qt.SizeBDiagCursor }

    component Grip: MouseArea {
        id: area

        property int edges: 0

        onPressed: mouse => root.begin(area, mouse)
        onPositionChanged: mouse => root.move(area, mouse, edges)
    }
}
