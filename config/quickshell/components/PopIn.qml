import QtQuick

Item {
    id: root

    property bool shown: false
    property int delay: 0
    default property alias content: body.data

    implicitHeight: body.childrenRect.height

    Item {
        id: body

        width: parent.width
        height: parent.height
        opacity: 0
        scale: 0.94

        transform: Translate {
            id: shift
            x: 10
        }

        states: State {
            name: "shown"
            when: root.shown

            PropertyChanges {
                target: body
                opacity: 1
                scale: 1
            }

            PropertyChanges {
                target: shift
                x: 0
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
                            target: body
                            property: "opacity"
                            duration: 120
                        }
                        NumberAnimation {
                            target: body
                            property: "scale"
                            duration: 240
                            easing.type: Easing.OutBack
                            easing.overshoot: 1.4
                        }
                        NumberAnimation {
                            target: shift
                            property: "x"
                            duration: 240
                            easing.type: Easing.OutCubic
                        }
                    }
                }
            },
            Transition {
                from: "shown"

                ParallelAnimation {
                    NumberAnimation {
                        targets: [body, shift]
                        properties: "opacity,scale,x"
                        duration: 90
                    }
                }
            }
        ]
    }
}
