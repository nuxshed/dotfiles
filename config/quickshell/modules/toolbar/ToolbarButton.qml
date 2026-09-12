import QtQuick
import "../../components"
import "../../config"

Item {
    id: root

    property string icon: ""
    property string tooltip: ""
    property bool highlighted: false
    property bool placeholder: false
    property bool shown: false
    property bool divider: false
    property int delay: 0
    property point probe: Qt.point(-1, -1)

    readonly property bool hovered: {
        if (probe.x < 0)
            return false;
        const p = mapFromItem(null, probe.x, probe.y);
        return p.x >= 0 && p.y >= 0 && p.x < width && p.y < height;
    }

    implicitWidth: 32
    implicitHeight: 32

    Rectangle {
        visible: root.divider
        x: -6
        anchors.verticalCenter: parent.verticalCenter
        width: 1
        height: 18
        color: Colors.border
        opacity: root.shown ? 1 : 0

        Behavior on opacity {
            NumberAnimation {
                duration: 200
            }
        }
    }

    Item {
        id: visual

        anchors.fill: parent
        opacity: 0
        scale: 0.5

        states: State {
            name: "shown"
            when: root.shown

            PropertyChanges {
                target: visual
                opacity: 1
                scale: 1
            }
        }

        transitions: [
            Transition {
                to: "shown"

                SequentialAnimation {
                    PauseAnimation {
                        duration: root.delay
                    }

                    ParallelAnimation {
                        NumberAnimation {
                            property: "opacity"
                            duration: 130
                        }
                        NumberAnimation {
                            property: "scale"
                            duration: 320
                            easing.type: Easing.OutBack
                            easing.overshoot: 2.6
                        }
                    }
                }
            },
            Transition {
                from: "shown"

                ParallelAnimation {
                    NumberAnimation {
                        property: "opacity"
                        duration: 90
                    }
                    NumberAnimation {
                        property: "scale"
                        duration: 130
                    }
                }
            }
        ]

        Rectangle {
            anchors.fill: parent
            radius: height / 2
            color: root.highlighted ? Colors.subtle : (root.hovered ? Colors.surfaceActive : "transparent")

            Behavior on color {
                ColorAnimation {
                    duration: 140
                }
            }
        }

        MaterialIcon {
            anchors.centerIn: parent
            text: root.icon
            size: 17
            color: root.placeholder ? Colors.textMuted : (root.highlighted || root.hovered ? Colors.textBright : Colors.textDimmed)
        }

        Rectangle {
            anchors.horizontalCenter: parent.horizontalCenter
            y: -height - 9
            width: tipLabel.implicitWidth + 14
            height: 22
            radius: 6
            color: Colors.background
            opacity: root.hovered ? 1 : 0
            visible: opacity > 0
            z: 10

            Behavior on opacity {
                NumberAnimation {
                    duration: 140
                }
            }

            Text {
                id: tipLabel
                anchors.centerIn: parent
                text: root.tooltip
                color: Colors.text
                font.pixelSize: 11
            }
        }
    }
}
