pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Bluetooth as Bluez

/**
 * Bluetooth service backed by BlueZ.
 */
Singleton {
    id: root

    readonly property Bluez.BluetoothAdapter adapter: Bluez.Bluetooth.defaultAdapter
    readonly property bool available: adapter !== null
    readonly property bool enabled: adapter?.enabled ?? false
    readonly property bool scanning: adapter?.discovering ?? false
    readonly property var devices: adapter ? adapter.devices.values : []
    readonly property var connectedDevices: devices.filter(d => d.connected)

    function toggleEnabled() {
        if (adapter)
            adapter.enabled = !adapter.enabled
    }

    function scan() {
        if (adapter)
            adapter.discovering = !adapter.discovering
    }

    function activate(device) {
        if (device.connected)
            device.disconnect()
        else if (device.paired)
            device.connect()
        else
            device.pair()
    }
}
