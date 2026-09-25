pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import "../../components"
import "../../config"
import "../../services"
import "../preview"

FloatingWindow {
    id: root

    readonly property string heading: {
        const c = Calendar.cursor;
        if (Calendar.view === "month")
            return Qt.formatDate(c, "MMMM yyyy");
        if (Calendar.view === "day")
            return Qt.formatDate(c, "dddd, d MMMM yyyy");
        const start = new Date(c);
        start.setDate(c.getDate() - (c.getDay() + 6) % 7);
        const end = new Date(start);
        end.setDate(start.getDate() + 6);
        return start.getMonth() === end.getMonth()
            ? Qt.formatDate(start, "d") + " – " + Qt.formatDate(end, "d MMMM yyyy")
            : Qt.formatDate(start, "d MMM") + " – " + Qt.formatDate(end, "d MMM yyyy");
    }

    title: "Calendar"
    visible: Calendar.open
    implicitWidth: 1280
    implicitHeight: 800
    color: Colors.background

    onVisibleChanged: if (visible)
        keyScope.forceActiveFocus()

    Item {
        id: keyScope

        anchors.fill: parent
        focus: true

        Keys.onPressed: event => {
            const typing = search.input.activeFocus || Calendar.editing !== null;
            if (event.key === Qt.Key_Escape) {
                if (search.input.activeFocus) {
                    Calendar.query = "";
                    search.text = "";
                    keyScope.forceActiveFocus();
                } else if (Calendar.editing)
                    Calendar.editing = null;
                else
                    Calendar.open = false;
            } else if (typing)
                return;
            else if (event.key === Qt.Key_Tab)
                Calendar.cycleView(1);
            else if (event.key === Qt.Key_Backtab)
                Calendar.cycleView(-1);
            else if (event.key === Qt.Key_T)
                Calendar.today();
            else if (event.key === Qt.Key_Left || event.key === Qt.Key_H)
                Calendar.step(-1);
            else if (event.key === Qt.Key_Right || event.key === Qt.Key_L)
                Calendar.step(1);
            else if (event.key === Qt.Key_M)
                Calendar.view = "month";
            else if (event.key === Qt.Key_W)
                Calendar.view = "week";
            else if (event.key === Qt.Key_D)
                Calendar.view = "day";
            else if (event.key === Qt.Key_N) {
                const d = new Date(Calendar.selected);
                d.setHours(9, 0, 0, 0);
                Calendar.draft(d.getTime(), false);
            } else if (event.key === Qt.Key_Slash)
                search.input.forceActiveFocus();
            else
                return;
            event.accepted = true;
        }

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 16
            spacing: 16

            RowLayout {
                Layout.fillWidth: true
                Layout.preferredHeight: 34
                spacing: 6

                TextButton {
                    text: "Today"
                    implicitHeight: 30
                    onClicked: Calendar.today()
                }

                PreviewButton {
                    implicitWidth: 30
                    implicitHeight: 30
                    icon: "chevron_left"
                    onClicked: Calendar.step(-1)
                }

                PreviewButton {
                    implicitWidth: 30
                    implicitHeight: 30
                    icon: "chevron_right"
                    onClicked: Calendar.step(1)
                }

                Text {
                    Layout.leftMargin: 6
                    text: root.heading
                    color: Colors.textBright
                    font.pixelSize: 18
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }

                Item {
                    Layout.fillWidth: true
                }

                Segmented {
                    Layout.preferredWidth: 210
                    implicitHeight: 30
                    items: ["Month", "Week", "Day"]
                    currentIndex: ["month", "week", "day"].indexOf(Calendar.view)
                    onSelected: index => Calendar.view = ["month", "week", "day"][index]
                }

                Field {
                    id: search

                    Layout.preferredWidth: 200
                    implicitHeight: 30
                    icon: "search"
                    placeholder: "Search events"
                    onTextChanged: Calendar.query = text
                    onAccepted: if (Calendar.results.length > 0) {
                        Calendar.goto(new Date(Calendar.results[0].start));
                        Calendar.edit(Calendar.results[0]);
                    }
                    onEscaped: {
                        text = "";
                        keyScope.forceActiveFocus();
                    }
                }

                TextButton {
                    text: "New event"
                    primary: true
                    implicitHeight: 30
                    onClicked: {
                        const d = new Date(Calendar.selected);
                        d.setHours(9, 0, 0, 0);
                        Calendar.draft(d.getTime(), false);
                    }
                }

            }

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: 16

                Sidebar {
                    Layout.preferredWidth: 240
                    Layout.fillWidth: false
                    Layout.fillHeight: true
                }

                Item {
                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    MonthView {
                        anchors.fill: parent
                        visible: Calendar.view === "month" && Calendar.query.trim().length === 0
                    }

                    WeekView {
                        anchors.fill: parent
                        visible: Calendar.view !== "month" && Calendar.query.trim().length === 0
                        days: Calendar.view === "day" ? 1 : 7
                    }

                    SearchResults {
                        anchors.fill: parent
                        visible: Calendar.query.trim().length > 0
                    }
                }

                EventEditor {
                    Layout.preferredWidth: 300
                    Layout.fillWidth: false
                    Layout.fillHeight: true
                    visible: Calendar.editing !== null
                }
            }
        }
    }

    component TextButton: Rectangle {
        id: tb

        property string text: ""
        property bool primary: false

        signal clicked

        implicitWidth: tbLabel.implicitWidth + 24
        implicitHeight: 30
        radius: 8
        color: tb.primary ? Colors.primary : tbArea.containsMouse ? Colors.surfaceActive : Colors.surface
        opacity: tb.primary && tbArea.containsMouse ? 0.88 : 1

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        Text {
            id: tbLabel
            anchors.centerIn: parent
            text: tb.text
            color: tb.primary ? Colors.primaryText : Colors.text
            font.pixelSize: 12
            font.family: Fonts.family
            font.weight: tb.primary ? Font.Medium : Font.Normal
        }

        MouseArea {
            id: tbArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: tb.clicked()
        }
    }
}
