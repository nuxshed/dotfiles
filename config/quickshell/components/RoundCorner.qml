import QtQuick

Item {
    id: root

    enum CornerEnum { TopLeft, TopRight, BottomLeft, BottomRight }

    property int corner: RoundCorner.CornerEnum.TopLeft
    property real size: 16
    property color color: "transparent"

    readonly property bool isTop: corner === RoundCorner.CornerEnum.TopLeft || corner === RoundCorner.CornerEnum.TopRight
    readonly property bool isLeft: corner === RoundCorner.CornerEnum.TopLeft || corner === RoundCorner.CornerEnum.BottomLeft

    implicitWidth: size
    implicitHeight: size
    clip: true

    Rectangle {
        width: root.size * 4
        height: root.size * 4
        radius: root.size * 2
        antialiasing: true
        color: "transparent"
        border.color: root.color
        border.width: root.size
        x: root.isLeft ? -root.size : -root.size * 2
        y: root.isTop ? -root.size : -root.size * 2
    }
}
