import QtQuick
import QtQuick.Layouts
import "../../../services"
import "../../../config"
import "../../../components"

Popout {
    id: root

    property int tab: 0

    readonly property int listHeight: 136
    readonly property int trailingRows: 56 + (Network.wiredAvailable ? 56 : 0)

    readonly property var wifiList: Network.networks
        .filter(n => n.ssid.length > 0)
        .sort((a, b) => (b.active - a.active) || (b.strength - a.strength))

    readonly property var btList: Bluetooth.devices
        .filter(d => d.name.length > 0)
        .sort((a, b) => (b.connected - a.connected) || (b.paired - a.paired) || a.name.localeCompare(b.name))


    notch: 18
    contentHeight: column.implicitHeight + 40

    onOpened: {
        Network.updateNetwork()
        Network.getWifiStatus()
        Network.getSavedConnections()
        Vpn.check()
    }

    function promptPassword(ssid) {
        Prompt.ask({
            title: "Connect to " + ssid,
            subtitle: "This network is secured",
            placeholder: "Network password",
            action: "Connect",
            onSubmit: password => Network.connectToNetwork(ssid, password)
        })
    }

    function btStatus(device) {
        return device.connected ? "Connected"
            : device.pairing ? "Pairing…"
            : device.paired ? "Paired" : "Available"
    }

    function activateNetwork(ap) {
        if (ap.active)
            Network.disconnectFromNetwork()
        else if (ap.isSaved || !ap.isSecure)
            Network.connectToNetwork(ap.ssid, "")
        else
            root.promptPassword(ap.ssid)
    }

    component SectionHeader: RowLayout {
        id: header

        property string label: ""
        property bool busy: false
        property bool on: false

        signal refresh()
        signal toggled()

        Layout.fillWidth: true
        spacing: 10

        Text {
            text: header.label
            color: Colors.textBright
            font.pixelSize: 13
            font.family: Fonts.family
            font.weight: Font.Medium
        }

        Item { Layout.fillWidth: true }

        Rectangle {
            implicitWidth: 26
            implicitHeight: 26
            radius: height / 2
            color: refreshHover.hovered ? Colors.surfaceActive : Colors.surface

            Behavior on color {
                ColorAnimation { duration: 150 }
            }

            Text {
                id: refreshGlyph
                anchors.centerIn: parent
                text: "↻"
                color: Colors.textBright
                font.pixelSize: 13
                font.family: Fonts.family
            }

            NumberAnimation {
                target: refreshGlyph
                property: "rotation"
                running: header.busy
                from: 0
                to: 360
                duration: 900
                loops: Animation.Infinite
                onStopped: refreshGlyph.rotation = 0
            }

            HoverHandler {
                id: refreshHover
                cursorShape: Qt.PointingHandCursor
            }

            TapHandler {
                onTapped: header.refresh()
            }
        }

        Toggle {
            checked: header.on
            onToggled: header.toggled()
        }
    }

    component ListCard: Rectangle {
        id: card

        default property alias content: holder.data

        property string placeholder: ""
        property bool empty: false

        Layout.fillWidth: true
        Layout.preferredHeight: root.listHeight
        radius: 16
        color: Colors.surface
        clip: true

        Item {
            id: holder
            anchors.fill: parent
            anchors.margins: 6
        }

        Text {
            anchors.centerIn: parent
            visible: card.empty
            text: card.placeholder
            color: Colors.textMuted
            font.pixelSize: 11
            font.family: Fonts.family
        }
    }

    component ToggleRow: Rectangle {
        id: row

        property string label: ""
        property string detail: ""
        property bool on: false

        signal toggled()

        Layout.fillWidth: true
        Layout.preferredHeight: 44
        radius: 12
        color: Colors.surface

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 14
            anchors.rightMargin: 14
            spacing: 10

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 1

                Text {
                    text: row.label
                    color: Colors.text
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                Text {
                    visible: row.detail.length > 0
                    text: row.detail
                    color: Colors.textMuted
                    font.pixelSize: 9
                    font.family: Fonts.family
                    elide: Text.ElideRight
                    Layout.fillWidth: true
                }
            }

            Toggle {
                checked: row.on
                onToggled: row.toggled()
            }
        }
    }

    ColumnLayout {
        id: column
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 20
        spacing: 14

        Segmented {
            Layout.fillWidth: true
            items: ["Wi-Fi", "Bluetooth"]
            currentIndex: root.tab
            onSelected: index => root.tab = index
        }

        ColumnLayout {
            Layout.fillWidth: true
            visible: root.tab === 0
            spacing: 12

            SectionHeader {
                label: Network.ssid || "Wi-Fi"
                busy: Network.scanning
                on: Network.wifiEnabled
                onRefresh: Network.rescanWifi()
                onToggled: Network.toggleWifi()
            }

            ListCard {
                empty: root.wifiList.length === 0
                placeholder: Network.wifiEnabled ? "No networks found" : "Wi-Fi is off"

                ListView {
                    anchors.fill: parent
                    spacing: 2
                    clip: true
                    boundsBehavior: Flickable.StopAtBounds
                    model: root.wifiList

                    delegate: Rectangle {
                        id: apRow

                        required property var modelData

                        width: ListView.view.width
                        height: 40
                        radius: 10
                        color: apHover.hovered ? Colors.surfaceActive : "transparent"

                        Behavior on color {
                            ColorAnimation { duration: 150 }
                        }

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: 10
                            anchors.rightMargin: 12
                            spacing: 10

                            SignalBars {
                                Layout.alignment: Qt.AlignVCenter
                                level: Math.ceil(apRow.modelData.strength / 25)
                                tint: apRow.modelData.active ? Colors.primary : Colors.textBright
                            }

                            ColumnLayout {
                                Layout.fillWidth: true
                                spacing: 1

                                Text {
                                    Layout.fillWidth: true
                                    text: apRow.modelData.ssid
                                    color: apRow.modelData.active ? Colors.primary : Colors.text
                                    font.pixelSize: 11
                                    font.family: Fonts.family
                                    font.weight: apRow.modelData.active ? Font.Medium : Font.Normal
                                    elide: Text.ElideRight
                                }

                                Text {
                                    text: apRow.modelData.active ? "Connected"
                                        : apRow.modelData.isSaved ? "Saved"
                                        : apRow.modelData.isSecure ? "Secured" : "Open"
                                    color: Colors.textMuted
                                    font.pixelSize: 9
                                    font.family: Fonts.family
                                }
                            }
                        }

                        HoverHandler {
                            id: apHover
                            cursorShape: Qt.PointingHandCursor
                        }

                        TapHandler {
                            onTapped: root.activateNetwork(apRow.modelData)
                        }
                    }
                }
            }

            ToggleRow {
                visible: Network.wiredAvailable
                label: "Ethernet"
                detail: Network.isWiredConnected ? Network.wiredDevice : "Disconnected"
                on: Network.isWiredConnected
                onToggled: Network.isWiredConnected ? Network.disconnectWired() : Network.connectWired()
            }

            ToggleRow {
                label: "VPN"
                detail: Vpn.error || Vpn.name
                on: Vpn.active
                onToggled: Prompt.ask({
                    title: Vpn.active ? "Stop VPN" : "Start VPN",
                    subtitle: Vpn.name,
                    placeholder: "sudo password",
                    action: Vpn.active ? "Stop" : "Start",
                    onSubmit: password => Vpn.toggle(password)
                })
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            visible: root.tab === 1
            spacing: 12

            SectionHeader {
                label: "Bluetooth"
                busy: Bluetooth.scanning
                on: Bluetooth.enabled
                onRefresh: Bluetooth.scan()
                onToggled: Bluetooth.toggleEnabled()
            }

            ListCard {
                Layout.preferredHeight: root.listHeight + root.trailingRows
                empty: root.btList.length === 0
                placeholder: !Bluetooth.available ? "No adapter"
                    : Bluetooth.enabled ? "No devices found" : "Bluetooth is off"

                ListView {
                    anchors.fill: parent
                    spacing: 2
                    clip: true
                    boundsBehavior: Flickable.StopAtBounds
                    model: root.btList

                    delegate: Rectangle {
                        id: btRow

                        required property var modelData

                        width: ListView.view.width
                        height: 40
                        radius: 10
                        color: btHover.hovered ? Colors.surfaceActive : "transparent"

                        Behavior on color {
                            ColorAnimation { duration: 150 }
                        }

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: 12
                            anchors.rightMargin: 12
                            spacing: 10

                            ColumnLayout {
                                Layout.fillWidth: true
                                spacing: 1

                                Text {
                                    Layout.fillWidth: true
                                    text: btRow.modelData.name
                                    color: btRow.modelData.connected ? Colors.primary : Colors.text
                                    font.pixelSize: 11
                                    font.family: Fonts.family
                                    font.weight: btRow.modelData.connected ? Font.Medium : Font.Normal
                                    elide: Text.ElideRight
                                }

                                Text {
                                    text: root.btStatus(btRow.modelData)
                                    color: Colors.textMuted
                                    font.pixelSize: 9
                                    font.family: Fonts.family
                                }
                            }
                        }

                        HoverHandler {
                            id: btHover
                            cursorShape: Qt.PointingHandCursor
                        }

                        TapHandler {
                            onTapped: Bluetooth.activate(btRow.modelData)
                        }
                    }
                }
            }
        }
    }
}
