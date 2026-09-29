pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Page {
    id: root

    readonly property var networks: Network.networks.filter(n => n.ssid.length > 0).sort((a, b) => b.active - a.active || b.strength - a.strength).slice(0, 8)
    readonly property var devices: Bluetooth.devices.filter(d => d.paired || d.connected).sort((a, b) => b.connected - a.connected)

    title: "Network"
    subtitle: Network.isWiredConnected ? "Connected by cable" : Network.ssid ? "Connected to " + Network.ssid : "Not connected"

    Component.onCompleted: {
        Network.updateNetwork();
        Network.getWifiStatus();
        Network.getSavedConnections();
    }

    function activate(ap: var): void {
        if (ap.active)
            Network.disconnectFromNetwork();
        else if (ap.isSaved || !ap.isSecure)
            Network.connectToNetwork(ap.ssid, "");
        else
            Prompt.ask({
                title: "Connect to " + ap.ssid,
                subtitle: "This network is secured",
                placeholder: "Network password",
                action: "Connect",
                onSubmit: password => Network.connectToNetwork(ap.ssid, password)
            });
    }

    Group {
        title: "Wi-Fi"

        SettingRow {
            icon: Network.wifiEnabled ? "wifi" : "wifi_off"
            label: "Wi-Fi"
            description: Network.scanning ? "Scanning…" : Network.wifiEnabled ? root.networks.length + " networks nearby" : "Off"

            Button {
                visible: Network.wifiEnabled
                icon: "refresh"
                onClicked: Network.rescanWifi()
            }

            Toggle {
                anchors.verticalCenter: parent.verticalCenter
                checked: Network.wifiEnabled
                onToggled: Network.toggleWifi()
            }
        }

        Repeater {
            model: Network.wifiEnabled ? root.networks : []

            SettingRow {
                required property var modelData

                icon: modelData.isSecure ? "lock" : "lock_open"
                label: modelData.ssid
                description: (modelData.active ? "Connected · " : modelData.isSaved ? "Saved · " : "") + modelData.strength + "%"
                clickable: true
                selected: modelData.active
                onClicked: root.activate(modelData)
            }
        }
    }

    Group {
        title: "Bluetooth"
        visible: Bluetooth.available

        SettingRow {
            icon: Bluetooth.enabled ? "bluetooth" : "bluetooth_disabled"
            label: "Bluetooth"
            description: !Bluetooth.enabled ? "Off" : Bluetooth.connectedDevices.length > 0 ? Bluetooth.connectedDevices.map(d => d.name).join(", ") : "No devices connected"

            Toggle {
                checked: Bluetooth.enabled
                onToggled: Bluetooth.toggleEnabled()
            }
        }

        Repeater {
            model: Bluetooth.enabled ? root.devices : []

            SettingRow {
                required property var modelData

                icon: "devices_other"
                label: modelData.name
                description: modelData.connected ? "Connected" + (modelData.batteryAvailable ? " · " + Math.round(modelData.battery * 100) + "%" : "") : modelData.pairing ? "Pairing…" : "Paired"
                clickable: true
                selected: modelData.connected
                onClicked: Bluetooth.activate(modelData)
            }
        }
    }
}
