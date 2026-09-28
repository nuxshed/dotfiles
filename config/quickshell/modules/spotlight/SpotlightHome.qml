pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import "../../components"
import "../../config"
import "../../services"
import "../../services/spotlight"

ColumnLayout {
    id: root

    readonly property bool media: Mpris.hasActivePlayer && Mpris.trackTitle.length > 0
    readonly property real now: clock.date.getTime()
    readonly property var next: Calendar.events
        .filter(e => !e.allDay && e.end > root.now && e.start - root.now < 12 * 3600000)
        .sort((a, b) => a.start - b.start)[0] ?? null
    readonly property bool companion: root.media || root.next !== null
    readonly property var timers: Timers.items.filter(t => t.running || t.done)
    readonly property bool lowBattery: Battery.isAvailable && !Battery.isCharging && Battery.level <= 20
    readonly property bool chips: (root.media && root.next !== null) || root.timers.length > 0 || Pomodoro.active || Recorder.active || root.lowBattery

    function until(e: var): string {
        const mins = Math.round((e.start - root.now) / 60000);
        if (mins <= 0)
            return "now · until " + Qt.formatTime(new Date(e.end), "HH:mm");
        const h = Math.floor(mins / 60);
        return "in " + (h > 0 ? h + "h " + (mins % 60) + "m" : mins + "m") + " · " + Qt.formatTime(new Date(e.start), "HH:mm");
    }

    function hour(at: int): string {
        return String(Math.floor(at / 100) % 24).padStart(2, "0") + ":00";
    }

    spacing: 10

    SystemClock {
        id: clock
        precision: SystemClock.Minutes
    }

    RowLayout {
        Layout.fillWidth: true
        Layout.preferredHeight: 92
        spacing: 10

        Tile {
            Layout.fillWidth: !root.companion
            Layout.preferredWidth: 250
            Layout.fillHeight: true

            ColumnLayout {
                anchors.left: parent.left
                anchors.verticalCenter: parent.verticalCenter
                anchors.leftMargin: 16
                spacing: 0

                Text {
                    text: Qt.formatTime(clock.date, "HH:mm")
                    color: Colors.textBright
                    font.pixelSize: 30
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }

                Text {
                    text: Qt.formatDate(clock.date, "dddd, d MMMM")
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }
            }

            RowLayout {
                anchors.right: parent.right
                anchors.verticalCenter: parent.verticalCenter
                anchors.rightMargin: 16
                visible: Weather.ready
                spacing: 18

                Repeater {
                    model: root.companion ? [] : Weather.hours

                    ColumnLayout {
                        id: hourCol

                        required property var modelData

                        spacing: 2

                        Text {
                            Layout.alignment: Qt.AlignHCenter
                            text: root.hour(hourCol.modelData.at)
                            color: Colors.textMuted
                            font.pixelSize: 9
                            font.family: Fonts.family
                        }

                        MaterialIcon {
                            Layout.alignment: Qt.AlignHCenter
                            text: Weather.iconFor(hourCol.modelData.code, hourCol.modelData.at % 2400 >= 1900 || hourCol.modelData.at % 2400 < 600)
                            size: 16
                            color: Colors.textDimmed
                        }

                        Text {
                            Layout.alignment: Qt.AlignHCenter
                            text: hourCol.modelData.temp + "°"
                            color: Colors.text
                            font.pixelSize: 10
                            font.family: Fonts.family
                        }
                    }
                }

                ColumnLayout {
                    spacing: 1

                    RowLayout {
                        Layout.alignment: Qt.AlignRight
                        spacing: 6

                        MaterialIcon {
                            text: Weather.icon
                            size: 20
                            color: Colors.yellow
                        }

                        Text {
                            text: Weather.temp + "°"
                            color: Colors.textBright
                            font.pixelSize: 20
                            font.family: Fonts.family
                        }
                    }

                    Text {
                        Layout.alignment: Qt.AlignRight
                        text: Weather.desc
                        color: Colors.textDimmed
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }

                    Text {
                        Layout.alignment: Qt.AlignRight
                        text: "H " + Weather.high + "°  L " + Weather.low + "°"
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }
                }
            }
        }

        Tile {
            visible: root.media && root.next === null
            Layout.fillWidth: true
            Layout.fillHeight: true

            RowLayout {
                anchors.fill: parent
                anchors.margins: 10
                spacing: 12

                ClippingRectangle {
                    Layout.preferredWidth: 72
                    Layout.preferredHeight: 72
                    radius: 8
                    color: Colors.subtle

                    MaterialIcon {
                        anchors.centerIn: parent
                        text: "music_note"
                        size: 24
                        color: Colors.textMuted
                    }

                    Image {
                        anchors.fill: parent
                        source: Mpris.artworkUrl
                        sourceSize: Qt.size(144, 144)
                        fillMode: Image.PreserveAspectCrop
                        asynchronous: true
                    }
                }

                ColumnLayout {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    spacing: 1

                    Text {
                        Layout.fillWidth: true
                        text: Mpris.trackTitle
                        color: Colors.textBright
                        font.pixelSize: 13
                        font.family: Fonts.family
                        font.weight: Font.Medium
                        elide: Text.ElideRight
                    }

                    Text {
                        Layout.fillWidth: true
                        text: Mpris.trackArtist
                        color: Colors.textMuted
                        font.pixelSize: 11
                        font.family: Fonts.family
                        elide: Text.ElideRight
                    }

                    Item {
                        Layout.fillHeight: true
                    }

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 4

                        Rectangle {
                            Layout.fillWidth: true
                            Layout.preferredHeight: 3
                            Layout.rightMargin: 8
                            radius: 1.5
                            color: Colors.subtle

                            Rectangle {
                                width: parent.width * Math.max(0, Math.min(1, Mpris.progress))
                                height: parent.height
                                radius: 1.5
                                color: Colors.primary
                            }
                        }

                        IconButton {
                            icon: "skip_previous"
                            onActivated: Mpris.previous()
                        }

                        IconButton {
                            icon: Mpris.isPlaying ? "pause" : "play_arrow"
                            onActivated: Mpris.togglePlayPause()
                        }

                        IconButton {
                            icon: "skip_next"
                            onActivated: Mpris.next()
                        }
                    }
                }
            }
        }

        Tile {
            visible: root.next !== null
            Layout.fillWidth: true
            Layout.fillHeight: true

            MouseArea {
                anchors.fill: parent
                cursorShape: Qt.PointingHandCursor
                onClicked: {
                    Calendar.show(new Date(root.next.start));
                    Spotlight.hide();
                }
            }

            RowLayout {
                anchors.fill: parent
                anchors.margins: 14
                spacing: 12

                Rectangle {
                    Layout.preferredWidth: 3
                    Layout.fillHeight: true
                    radius: 1.5
                    color: root.next?.color ?? Colors.primary
                }

                ColumnLayout {
                    Layout.fillWidth: true
                    spacing: 2

                    Text {
                        text: "Up next"
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                        font.capitalization: Font.AllUppercase
                        font.letterSpacing: 0.6
                    }

                    Text {
                        Layout.fillWidth: true
                        text: root.next?.summary ?? ""
                        color: Colors.textBright
                        font.pixelSize: 14
                        font.family: Fonts.family
                        font.weight: Font.Medium
                        elide: Text.ElideRight
                    }

                    Text {
                        text: root.next ? root.until(root.next) : ""
                        color: Colors.textDimmed
                        font.pixelSize: 11
                        font.family: Fonts.family
                    }
                }
            }
        }
    }

    Flow {
        Layout.fillWidth: true
        visible: root.chips
        spacing: 6

        Chip {
            visible: root.media && root.next !== null
            icon: Mpris.isPlaying ? "pause" : "play_arrow"
            tint: Colors.green
            label: Mpris.trackTitle + (Mpris.trackArtist ? " · " + Mpris.trackArtist : "")
            onActivated: Mpris.togglePlayPause()
        }

        Repeater {
            model: root.timers

            Chip {
                required property var modelData

                icon: modelData.kind === "timer" ? "hourglass_empty" : "timer"
                tint: modelData.done ? Colors.red : Colors.primary
                label: modelData.done ? "Timer done" : Timers.clock(modelData.kind === "timer" ? Timers.remaining(modelData) : Timers.elapsed(modelData), false)
                onActivated: modelData.done ? Timers.remove(modelData.uid) : Timers.toggle(modelData.uid)
            }
        }

        Chip {
            visible: Pomodoro.active
            icon: "timer"
            tint: Pomodoro.status === "break" ? Colors.green : Colors.primary
            label: Pomodoro.label + " · " + Pomodoro.display
            onActivated: Pomodoro.toggle()
        }

        Chip {
            visible: Recorder.active
            icon: "fiber_manual_record"
            tint: Colors.red
            label: (Recorder.kind === "voice" ? "Voice" : "Screen") + " recording · stop"
            onActivated: Recorder.stop()
        }

        Chip {
            visible: root.lowBattery
            icon: "battery_alert"
            tint: Colors.yellow
            label: Battery.level + "% battery" + (Battery.timeRemaining ? " · " + Battery.timeRemaining + " left" : "")
        }
    }

    Text {
        Layout.leftMargin: 4
        Layout.topMargin: 2
        text: "Suggested"
        color: Colors.textMuted
        font.pixelSize: 10
        font.family: Fonts.family
        font.capitalization: Font.AllUppercase
        font.letterSpacing: 0.6
    }

    RowLayout {
        Layout.fillWidth: true
        spacing: 6

        Repeater {
            model: Spotlight.suggestions

            Rectangle {
                id: app

                required property var modelData
                required property int index

                readonly property bool current: app.index === Spotlight.selected
                readonly property string themed: (app.modelData.iconIsImage ?? false) ? Quickshell.iconPath(app.modelData.icon ?? "", true) : ""

                Layout.fillWidth: true
                Layout.preferredHeight: 78
                radius: 10
                color: app.current ? Colors.surfaceActive : appArea.containsMouse ? Colors.surface : "transparent"

                Behavior on color {
                    ColorAnimation { duration: 120 }
                }

                ColumnLayout {
                    anchors.centerIn: parent
                    width: parent.width - 12
                    spacing: 6

                    Item {
                        Layout.alignment: Qt.AlignHCenter
                        Layout.preferredWidth: 36
                        Layout.preferredHeight: 36

                        IconImage {
                            anchors.fill: parent
                            visible: app.themed.length > 0
                            source: app.themed
                        }

                        MaterialIcon {
                            anchors.centerIn: parent
                            visible: app.themed.length === 0
                            text: (app.modelData.iconIsImage ?? false) ? (app.modelData.fallbackIcon ?? "apps") : (app.modelData.icon ?? "apps")
                            size: 26
                            color: Colors.textDimmed
                        }
                    }

                    Text {
                        Layout.fillWidth: true
                        text: app.modelData.title ?? ""
                        color: app.current ? Colors.textBright : Colors.textDimmed
                        font.pixelSize: 10
                        font.family: Fonts.family
                        horizontalAlignment: Text.AlignHCenter
                        elide: Text.ElideRight
                    }
                }

                MouseArea {
                    id: appArea

                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: {
                        Spotlight.selected = app.index;
                        Spotlight.activate(false);
                    }
                }
            }
        }
    }

    ColumnLayout {
        Layout.fillWidth: true
        Layout.topMargin: 2
        visible: Spotlight.selection.homeItems.length > 0
        spacing: 8

        RowLayout {
            Layout.fillWidth: true
            Layout.leftMargin: 4
            Layout.rightMargin: 4
            spacing: 10

            Text {
                text: Spotlight.selection.source === "primary" ? "Selection" : "Clipboard"
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
                font.capitalization: Font.AllUppercase
                font.letterSpacing: 0.6
            }

            Text {
                Layout.fillWidth: true
                text: "“" + Spotlight.selection.preview + "”"
                color: Colors.textDimmed
                font.pixelSize: 11
                font.family: Fonts.family
                elide: Text.ElideRight
            }

            Text {
                text: Spotlight.selection.stats
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
            }
        }

        Flow {
            Layout.fillWidth: true
            spacing: 6

            Repeater {
                model: Spotlight.selection.homeItems

                Chip {
                    required property var modelData
                    required property int index

                    readonly property int slot: Spotlight.suggestions.length + index

                    icon: modelData.icon
                    label: modelData.title
                    tint: Colors.textDimmed
                    current: Spotlight.selected === slot
                    onActivated: {
                        Spotlight.selected = slot;
                        Spotlight.activate(false);
                    }
                }
            }
        }
    }

    component Tile: Rectangle {
        radius: 12
        color: Colors.surface
    }

    component IconButton: MaterialIcon {
        id: btn

        property string icon
        signal activated

        text: btn.icon
        size: 18
        color: btnArea.containsMouse ? Colors.textBright : Colors.textDimmed

        MouseArea {
            id: btnArea

            anchors.fill: parent
            anchors.margins: -4
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: btn.activated()
        }
    }

    component Chip: Rectangle {
        id: chip

        property string icon
        property string label
        property color tint: Colors.primary
        property bool current: false
        signal activated

        implicitWidth: chipRow.implicitWidth + 20
        implicitHeight: 26
        radius: 13
        color: chip.current || chipArea.containsMouse ? Colors.surfaceActive : Colors.surface
        border.color: chip.current ? Colors.outline : "transparent"
        border.width: 1

        RowLayout {
            id: chipRow

            anchors.centerIn: parent
            spacing: 6

            MaterialIcon {
                text: chip.icon
                size: 14
                color: chip.tint
            }

            Text {
                text: chip.label
                color: Colors.text
                font.pixelSize: 11
                font.family: Fonts.family
            }
        }

        MouseArea {
            id: chipArea

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: chip.activated()
        }
    }
}
