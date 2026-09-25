pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    readonly property var ev: Calendar.editing
    readonly property bool readonly: ev?.readonly ?? false
    readonly property bool isNew: !ev || !ev.id

    property bool allDay: false
    property color tint: Colors.primary

    function load(): void {
        if (!root.ev)
            return;
        title.text = root.ev.summary ?? "";
        root.allDay = !!root.ev.allDay;
        root.tint = root.ev.color ?? Colors.primary;
        startDate.text = Qt.formatDate(new Date(root.ev.start), "yyyy-MM-dd");
        startTime.text = Qt.formatTime(new Date(root.ev.start), "HH:mm");
        const end = new Date(root.ev.end - (root.allDay ? 1 : 0));
        endDate.text = Qt.formatDate(end, "yyyy-MM-dd");
        endTime.text = Qt.formatTime(new Date(root.ev.end), "HH:mm");
        location.text = root.ev.location ?? "";
        notes.text = root.ev.notes ?? "";
        if (!root.readonly)
            title.input.forceActiveFocus();
    }

    function parse(date: string, time: string, endOfDay: bool): real {
        const m = date.trim().match(/^(\d{4})-(\d{1,2})-(\d{1,2})$/);
        if (!m)
            return NaN;
        const t = root.allDay ? [0, 0] : (time.trim().match(/^(\d{1,2}):(\d{2})$/) ?? [0, 0, 0]).slice(1).map(Number);
        const d = new Date(+m[1], +m[2] - 1, +m[3], t[0] ?? 0, t[1] ?? 0);
        if (root.allDay && endOfDay)
            d.setDate(d.getDate() + 1);
        return d.getTime();
    }

    function commit(): void {
        const start = root.parse(startDate.text, startTime.text, false);
        const end = root.parse(endDate.text, endTime.text, true);
        if (isNaN(start) || isNaN(end))
            return;
        Calendar.save({ id: root.ev.id, summary: title.text, start, end, allDay: root.allDay, location: location.text, notes: notes.text });
    }

    onEvChanged: root.load()

    radius: 12
    color: Colors.surface

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 14
        spacing: 10

        RowLayout {
            Layout.fillWidth: true

            Rectangle {
                width: 10
                height: 10
                radius: 3
                color: root.tint
            }

            Text {
                Layout.fillWidth: true
                text: root.readonly ? "Event" : root.isNew ? "New event" : "Edit event"
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            Small {
                icon: "close"
                onActivated: Calendar.editing = null
            }
        }

        Field {
            id: title

            Layout.fillWidth: true
            implicitHeight: 38
            placeholder: "Title"
            enabled: !root.readonly
            input.font.pixelSize: 15
            input.font.weight: Font.Medium
            onAccepted: root.commit()
        }

        RowLayout {
            Layout.fillWidth: true

            Text {
                Layout.fillWidth: true
                text: "All day"
                color: Colors.textDimmed
                font.pixelSize: 12
                font.family: Fonts.family
            }

            Toggle {
                enabled: !root.readonly
                checked: root.allDay
                onToggled: root.allDay = !root.allDay
            }
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: 6

            Field {
                id: startDate
                Layout.fillWidth: true
                label: "From"
                enabled: !root.readonly
                onAccepted: root.commit()
            }

            Field {
                id: startTime
                Layout.preferredWidth: 64
                visible: !root.allDay
                enabled: !root.readonly
                onAccepted: root.commit()
            }
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: 6

            Field {
                id: endDate
                Layout.fillWidth: true
                label: "To"
                enabled: !root.readonly
                onAccepted: root.commit()
            }

            Field {
                id: endTime
                Layout.preferredWidth: 64
                visible: !root.allDay
                enabled: !root.readonly
                onAccepted: root.commit()
            }
        }

        Field {
            id: location
            Layout.fillWidth: true
            icon: "place"
            placeholder: "Location"
            enabled: !root.readonly
            onAccepted: root.commit()
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            radius: 8
            color: Colors.background
            border.width: 1
            border.color: notes.activeFocus ? Colors.outline : "transparent"

            Flickable {
                anchors.fill: parent
                anchors.margins: 8
                contentHeight: notes.implicitHeight
                clip: true
                boundsBehavior: Flickable.StopAtBounds

                TextEdit {
                    id: notes

                    width: parent.width
                    wrapMode: TextEdit.Wrap
                    color: Colors.text
                    selectionColor: Colors.primaryContainer
                    selectedTextColor: Colors.textBright
                    font.pixelSize: 12
                    font.family: Fonts.family
                    selectByMouse: true
                    readOnly: root.readonly

                    Text {
                        visible: notes.text.length === 0
                        text: "Notes"
                        color: Colors.textMuted
                        font: notes.font
                    }
                }
            }
        }

        Text {
            visible: root.readonly
            Layout.fillWidth: true
            text: "Read-only · from a subscribed calendar"
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family
        }

        RowLayout {
            visible: !root.readonly
            Layout.fillWidth: true
            spacing: 6

            Button {
                visible: !root.isNew
                icon: "delete"
                tint: Colors.red
                onActivated: Calendar.remove(root.ev.id)
            }

            Item { Layout.fillWidth: true }

            Button {
                label: "Cancel"
                onActivated: Calendar.editing = null
            }

            Button {
                label: root.isNew ? "Create" : "Save"
                accent: true
                onActivated: root.commit()
            }
        }
    }

    component Small: Rectangle {
        id: btn

        property string icon: ""

        signal activated

        Layout.preferredWidth: 24
        Layout.preferredHeight: 24
        radius: 12
        color: area.containsMouse ? Colors.surfaceActive : "transparent"

        MaterialIcon {
            anchors.centerIn: parent
            text: btn.icon
            size: 14
            color: area.containsMouse ? Colors.textBright : Colors.textMuted
        }

        MouseArea {
            id: area
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: btn.activated()
        }
    }

    component Button: Rectangle {
        id: b

        property string icon: ""
        property string label: ""
        property bool accent: false
        property color tint: Colors.text

        signal activated

        implicitWidth: b.label.length > 0 ? bLabel.implicitWidth + 24 : 32
        implicitHeight: 32
        radius: 8
        color: b.accent ? Colors.primary : bArea.containsMouse ? Colors.surfaceActive : Colors.background
        opacity: b.accent && bArea.containsMouse ? 0.88 : 1

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        MaterialIcon {
            visible: b.icon.length > 0
            anchors.centerIn: parent
            text: b.icon
            size: 16
            color: b.tint
        }

        Text {
            id: bLabel
            visible: b.label.length > 0
            anchors.centerIn: parent
            text: b.label
            color: b.accent ? Colors.primaryText : Colors.text
            font.pixelSize: 12
            font.family: Fonts.family
            font.weight: b.accent ? Font.Medium : Font.Normal
        }

        MouseArea {
            id: bArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: b.activated()
        }
    }
}
