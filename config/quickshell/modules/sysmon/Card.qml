import QtQuick
import QtQuick.Layouts
import "../../config"
import "../../services"

Rectangle {
    id: root

    property string title: ""
    property string subtitle: ""
    property string value: ""
    property color accent: Colors.blue
    property var series: []
    property real max: 100
    property int span: -1
    property var fmt: null
    property bool clickable: false

    signal clicked

    radius: 10
    color: (hover.containsMouse || graph.hoverX >= 0) && root.clickable ? Colors.subtle : Colors.surfaceActive
    border.width: 1
    border.color: (hover.containsMouse || graph.hoverX >= 0) && root.clickable ? Colors.subtle : Colors.outline

    Behavior on color { ColorAnimation { duration: 120 } }
    Behavior on border.color { ColorAnimation { duration: 120 } }

    MouseArea {
        id: hover
        anchors.fill: parent
        enabled: root.clickable
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: root.clicked()
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 14
        spacing: 10

        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: false
            spacing: 8

            Text {
                Layout.fillWidth: true
                text: root.title
                color: Colors.textDimmed
                font.pixelSize: 13
                font.family: Fonts.family
                font.weight: Font.Medium
            }
            Text {
                text: root.value
                color: Colors.textBright
                font.pixelSize: 20
                font.family: Fonts.family
                font.weight: Font.Medium
            }
        }

        Text {
            Layout.fillWidth: true
            Layout.topMargin: -8
            text: root.subtitle
            color: Colors.textMuted
            font.pixelSize: 11
            font.family: Fonts.family
            elide: Text.ElideRight
        }

        Graph {
            id: graph
            Layout.fillWidth: true
            Layout.fillHeight: true
            series: root.series
            max: root.max
            grid: false
            fmt: root.fmt
            span: root.span < 0 ? SysMon.histLen : root.span
        }
    }

}
