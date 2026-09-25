pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    function esc(t: string): string {
        return t.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
    }

    function highlight(t: string, hits: var): string {
        let out = "";
        for (let i = 0; i < t.length; i++)
            out += hits.includes(i) ? "<b><font color=\"" + Colors.primary + "\">" + root.esc(t[i]) + "</font></b>" : root.esc(t[i]);
        return out;
    }

    radius: 12
    color: Colors.surface

    ListView {
        id: list

        anchors.fill: parent
        anchors.margins: 8
        clip: true
        header: Text {
            visible: list.count > 0
            width: list.width
            height: visible ? 30 : 0
            leftPadding: 10
            verticalAlignment: Text.AlignVCenter
            text: list.count + (list.count === 60 ? "+" : "") + (list.count === 1 ? " result" : " results")
            color: Colors.textMuted
            font.pixelSize: 11
            font.family: Fonts.family
        }
        spacing: 2
        boundsBehavior: Flickable.StopAtBounds
        model: Calendar.results

        delegate: Rectangle {
            id: row

            required property var modelData

            width: list.width
            height: 40
            opacity: row.modelData.upcoming ? 1 : 0.6
            radius: 8
            color: rowArea.containsMouse ? Colors.surfaceActive : "transparent"

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: 10
                anchors.rightMargin: 10
                spacing: 12

                Rectangle {
                    width: 4
                    height: 22
                    radius: 2
                    color: row.modelData.color
                }

                Text {
                    Layout.preferredWidth: 120
                    text: Qt.formatDate(new Date(row.modelData.start), "ddd d MMM yyyy")
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                Text {
                    Layout.preferredWidth: 90
                    text: row.modelData.allDay ? "all day" : Qt.formatTime(new Date(row.modelData.start), "HH:mm") + " – " + Qt.formatTime(new Date(row.modelData.end), "HH:mm")
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                Text {
                    Layout.fillWidth: true
                    text: root.highlight(row.modelData.summary, row.modelData.hits) + (row.modelData.location ? "<font color=\"" + Colors.textMuted + "\">  ·  " + root.esc(row.modelData.location) + "</font>" : "")
                    textFormat: Text.StyledText
                    color: Colors.text
                    font.pixelSize: 12
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }

                Text {
                    Layout.maximumWidth: 140
                    text: Calendar.sources.find(src => src.id === row.modelData.source)?.name ?? ""
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }
            }

            MouseArea {
                id: rowArea
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: {
                    Calendar.goto(new Date(row.modelData.start));
                    Calendar.edit(row.modelData);
                }
            }
        }

        Text {
            anchors.centerIn: parent
            visible: list.count === 0
            text: "No matching events"
            color: Colors.textMuted
            font.pixelSize: 12
            font.family: Fonts.family
        }
    }
}
