pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import "../../components"
import "../../config"
import "../../services"
import "../preview"
import "../sysmon" as SysMonUi

FloatingWindow {
    id: root

    readonly property var navItems: [
        { id: "appearance", icon: "palette", name: "Appearance" },
        { id: "wallpaper", icon: "wallpaper", name: "Wallpaper" },
        { id: "desktop", icon: "desktop_windows", name: "Desktop" },
        { id: "notifications", icon: "notifications", name: "Notifications" },
        { id: "sound", icon: "volume_up", name: "Sound" },
        { id: "display", icon: "brightness_6", name: "Display & lock" },
        { id: "network", icon: "wifi", name: "Network" }
    ]

    visible: SettingsApp.open
    implicitWidth: 980
    implicitHeight: 720
    minimumSize.width: 820
    minimumSize.height: 560
    color: "transparent"
    title: "Settings"

    onVisibleChanged: {
        if (visible)
            keyScope.forceActiveFocus();
        else
            SettingsApp.open = false;
    }

    Item {
        id: keyScope
        anchors.fill: parent
        focus: true

        Keys.onPressed: event => {
            if (event.key === Qt.Key_Escape) {
                SettingsApp.open = false;
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
                text: "Settings"
                color: Colors.textBright
                font.pixelSize: 17
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            PreviewButton {
                implicitWidth: 30
                implicitHeight: 30
                icon: "close"
                onClicked: SettingsApp.open = false
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true
            spacing: 14

            ColumnLayout {
                Layout.preferredWidth: 188
                Layout.fillWidth: false
                Layout.fillHeight: true
                Layout.leftMargin: 4
                spacing: 3

                Repeater {
                    model: root.navItems

                    SysMonUi.NavItem {
                        required property var modelData
                        Layout.fillWidth: true
                        icon: modelData.icon
                        text: modelData.name
                        active: SettingsApp.page === modelData.id
                        onClicked: SettingsApp.page = modelData.id
                    }
                }

                Item { Layout.fillHeight: true }

                SysMonUi.NavItem {
                    Layout.fillWidth: true
                    icon: "info_outline"
                    text: "About"
                    active: SettingsApp.page === "about"
                    onClicked: SettingsApp.page = "about"
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.fillHeight: true
                radius: 12
                color: Colors.surface
                border.width: 1
                border.color: Colors.border
                clip: true

                Loader {
                    id: loader

                    anchors.fill: parent
                    sourceComponent: {
                        switch (SettingsApp.page) {
                        case "wallpaper": return wallpaperPage;
                        case "desktop": return desktopPage;
                        case "notifications": return notificationsPage;
                        case "sound": return soundPage;
                        case "display": return displayPage;
                        case "network": return networkPage;
                        case "about": return aboutPage;
                        default: return appearancePage;
                        }
                    }

                    onLoaded: {
                        item.opacity = 0;
                        fadeIn.restart();
                    }

                    NumberAnimation {
                        id: fadeIn
                        target: loader.item
                        property: "opacity"
                        to: 1
                        duration: 180
                        easing.type: Easing.OutCubic
                    }
                }
            }
        }
    }

    Component { id: appearancePage; AppearancePage {} }
    Component { id: wallpaperPage; WallpaperPage {} }
    Component { id: desktopPage; DesktopPage {} }
    Component { id: notificationsPage; NotificationsPage {} }
    Component { id: soundPage; SoundPage {} }
    Component { id: displayPage; DisplayPage {} }
    Component { id: networkPage; NetworkPage {} }
    Component {
        id: aboutPage
        Item {
            SysMonUi.AboutPage {
                anchors.fill: parent
                anchors.margins: 16
            }
        }
    }
}
