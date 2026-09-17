import QtQuick
import QtQuick.Layouts
import "../../config"
import "../../services"

ColumnLayout {
    id: root

    property string title: ""
    property string subtitle: ""
    property string value: ""
    property string valueLabel: ""
    property var series: []
    property real max: 100
    property var legend: []
    property var fmt: null
    property var modes: []
    property int mode: 0
    property var stats: []
    property string extraTitle: ""
    default property alias extra: extraPanel.content

    spacing: 12

    PageHeader {
        title: root.title
        subtitle: root.subtitle
        value: root.value
        valueLabel: root.valueLabel
    }

    Rectangle {
        Layout.fillWidth: true
        Layout.fillHeight: true
        Layout.minimumHeight: 140
        radius: 10
        color: Colors.surfaceActive
        border.width: 1
        border.color: Colors.outline

        Graph {
            anchors.fill: parent
            anchors.margins: 14
            anchors.topMargin: 34
            series: root.series
            max: root.max
            fmt: root.fmt
        }

        Row {
            anchors.top: parent.top
            anchors.right: parent.right
            anchors.margins: 10
            spacing: 4
            visible: root.modes.length > 0

            Repeater {
                model: root.modes

                Rectangle {
                    id: chip
                    required property int index
                    required property string modelData
                    readonly property bool on: root.mode === index
                    width: chipText.width + 18
                    height: 22
                    radius: 6
                    color: on ? Colors.primaryContainer : chipMouse.containsMouse ? Colors.surfaceActive : "transparent"
                    border.width: 1
                    border.color: on ? Colors.primaryContainer : "transparent"

                    Text {
                        id: chipText
                        anchors.centerIn: parent
                        text: chip.modelData
                        color: chip.on ? Colors.primaryContainerText : Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }

                    MouseArea {
                        id: chipMouse
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: root.mode = chip.index
                    }
                }
            }
        }

        Text {
            anchors.top: parent.top
            anchors.right: parent.right
            anchors.margins: 14
            visible: root.modes.length === 0
            text: root.max > 0 ? `${root.max}%` : "auto scale"
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family
        }

        Text {
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            anchors.margins: 6
            anchors.leftMargin: 14
            text: `${SysMon.histLen} s`
            color: Colors.textMuted
            font.pixelSize: 9
            font.family: Fonts.family
        }

        Row {
            anchors.top: parent.top
            anchors.left: parent.left
            anchors.margins: 14
            spacing: 14

            Repeater {
                model: root.legend

                Row {
                    required property var modelData
                    spacing: 6

                    Rectangle {
                        width: 8
                        height: 8
                        radius: 4
                        color: parent.modelData.color
                        anchors.verticalCenter: parent.verticalCenter
                    }
                    Text {
                        text: parent.modelData.name
                        color: Colors.textMuted
                        font.pixelSize: 11
                        font.family: Fonts.family
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }
            }
        }
    }

    RowLayout {
        Layout.fillWidth: true
        Layout.fillHeight: false
        spacing: 12

        Panel {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.preferredWidth: 1
            title: "Details"

            Repeater {
                model: root.stats

                KeyValue {
                    required property var modelData
                    label: modelData.label
                    value: modelData.value
                }
            }
        }

        Panel {
            id: extraPanel
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.preferredWidth: 1
            visible: content.length > 0
            title: root.extraTitle
        }
    }
}
