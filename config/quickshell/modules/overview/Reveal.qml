import QtQuick

Item {
    id: root

    property bool shown: false
    property int delay: 0

    default property alias content: visual.data

    Item {
        id: visual

        anchors.fill: parent
        opacity: 0
        scale: 0.94

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
                    PauseAnimation { duration: root.delay }

                    ParallelAnimation {
                        NumberAnimation { property: "opacity"; duration: 160 }
                        NumberAnimation {
                            property: "scale"
                            duration: 340
                            easing.type: Easing.OutBack
                            easing.overshoot: 1.4
                        }
                    }
                }
            },
            Transition {
                from: "shown"

                ParallelAnimation {
                    NumberAnimation { property: "opacity"; duration: 90 }
                    NumberAnimation { property: "scale"; duration: 130 }
                }
            }
        ]
    }
}
