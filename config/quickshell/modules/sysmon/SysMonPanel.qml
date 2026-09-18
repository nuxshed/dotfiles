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

    readonly property var navItems: [
        { id: "performance", icon: "timeline", name: "Performance" },
        { id: "battery", icon: "battery_full", name: "Battery" },
        { id: "storage", icon: "sd_storage", name: "Storage" },
        { id: "sensors", icon: "whatshot", name: "Sensors" },
        { id: "processes", icon: "view_list", name: "Processes" }
    ]

    visible: SysMon.open
    implicitWidth: 960
    implicitHeight: 720
    minimumSize.width: 800
    minimumSize.height: 560
    color: "transparent"
    title: "System Monitor"

    onVisibleChanged: if (visible) keyScope.forceActiveFocus()

    Item {
        id: keyScope
        anchors.fill: parent
        focus: true

        Keys.onPressed: event => {
            if (event.key === Qt.Key_Escape) {
                SysMon.open = false;
                event.accepted = true;
            }
        }
    }

    WindowChrome { titleHeight: 52 }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        spacing: 12

        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: false
            Layout.preferredHeight: 40
            Layout.leftMargin: 14
            Layout.rightMargin: 4

            Text {
                Layout.fillWidth: true
                text: "System Monitor"
                color: Colors.textBright
                font.pixelSize: 17
                font.family: Fonts.family
                font.weight: Font.Medium
            }
            PreviewButton {
                implicitWidth: 30
                implicitHeight: 30
                icon: "close"
                onClicked: SysMon.open = false
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true
            spacing: 14

            ColumnLayout {
                Layout.preferredWidth: 176
                Layout.fillWidth: false
                Layout.fillHeight: true
                Layout.leftMargin: 4
                spacing: 3

                Repeater {
                    model: root.navItems

                    NavItem {
                        required property var modelData
                        Layout.fillWidth: true
                        icon: modelData.icon
                        text: modelData.name
                        active: SysMon.tab === modelData.id
                        onClicked: SysMon.tab = modelData.id
                    }
                }

                Item { Layout.fillHeight: true }

                Rectangle {
                    Layout.fillWidth: true
                    implicitHeight: 46
                    radius: 8
                    color: SysMon.tab === "about" ? Colors.surfaceActive : userMouse.containsMouse ? Colors.surface : "transparent"

                    Behavior on color { ColorAnimation { duration: 120 } }

                    Row {
                        anchors.verticalCenter: parent.verticalCenter
                        x: 8
                        spacing: 10

                        Rectangle {
                            width: 30
                            height: 30
                            radius: 15
                            color: Colors.subtle
                            anchors.verticalCenter: parent.verticalCenter

                            MaterialIcon {
                                anchors.centerIn: parent
                                text: "computer"
                                size: 15
                                color: Colors.textDimmed
                            }
                        }
                        Column {
                            spacing: 2
                            anchors.verticalCenter: parent.verticalCenter

                            Text {
                                text: Quickshell.env("USER") + "@" + SysMon.hostname
                                color: SysMon.tab === "about" ? Colors.textBright : Colors.text
                                font.pixelSize: 11
                                font.family: Fonts.family
                                font.weight: Font.Medium
                            }
                            Text {
                                text: "Up " + SysMon.fmtDuration(SysMon.uptime)
                                color: Colors.textMuted
                                font.pixelSize: 10
                                font.family: Fonts.family
                            }
                        }
                    }

                    MouseArea {
                        id: userMouse
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: SysMon.tab = "about"
                    }
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.fillHeight: true
                radius: 12
                color: Colors.surface
                border.width: 1
                border.color: Colors.border

                Loader {
                    anchors.fill: parent
                    anchors.margins: 16
                    sourceComponent: {
                        switch (SysMon.tab) {
                        case "battery": return batteryPage;
                        case "storage": return storagePage;
                        case "sensors": return sensorsPage;
                        case "about": return aboutPage;
                        case "processes": return processesPage;
                        default: return performancePage;
                        }
                    }
                }
            }
        }
    }

    Component { id: performancePage; PerformancePage {} }
    Component { id: batteryPage; BatteryPage {} }
    Component { id: storagePage; StoragePage {} }
    Component { id: sensorsPage; SensorsPage {} }
    Component { id: aboutPage; AboutPage {} }
    Component { id: processesPage; ProcessesPage {} }
}
