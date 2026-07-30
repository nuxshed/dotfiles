pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

/**
 * Bluetooth service using bluez via bluetoothctl.
 */
Singleton {
    id: root
    
    property bool enabled: false
    property bool scanning: false
    readonly property list<BluetoothDevice> devices: []
    property var deviceQueue: []

    function processNextDevice() {
        if (deviceQueue.length === 0 || getDeviceInfo.running) return
        var next = deviceQueue.shift()
        getDeviceInfo.deviceAddress = next.address
        getDeviceInfo.deviceName = next.name
        getDeviceInfo.running = true
    }
    
    function updateDevices() {
        getDevices.running = true
    }
    
    function enable() {
        enableProc.command = ["bluetoothctl", "power", "on"]
        enableProc.running = true
    }
    
    function disable() {
        enableProc.command = ["bluetoothctl", "power", "off"]
        enableProc.running = true
    }
    
    function toggleEnabled() {
        if (enabled) {
            disable()
        } else {
            enable()
        }
    }
    
    function startScan() {
        scanProc.command = ["bluetoothctl", "scan", "on"]
        scanProc.running = true
        scanning = true
    }
    
    function stopScan() {
        scanProc.command = ["bluetoothctl", "scan", "off"]
        scanProc.running = true
        scanning = false
    }
    
    function connectDevice(address) {
        connectProc.command = ["bluetoothctl", "connect", address]
        connectProc.running = true
    }
    
    function disconnectDevice(address) {
        disconnectProc.command = ["bluetoothctl", "disconnect", address]
        disconnectProc.running = true
    }
    
    function pairDevice(address) {
        pairProc.command = ["bluetoothctl", "pair", address]
        pairProc.running = true
    }
    
    Component.onCompleted: {
        updatePowerState()
        updateDevices()
    }
    
    function updatePowerState() {
        powerProc.running = true
    }
    
    Process {
        id: powerProc
        command: ["bluetoothctl", "show"]
        running: false
        
        stdout: StdioCollector {
            onStreamFinished: {
                var output = this.text
                var match = output.match(/Powered: (yes|no)/)
                if (match) {
                    root.enabled = match[1] === "yes"
                }
            }
        }
    }
    
    Process {
        id: enableProc
        running: false
        
        onExited: {
            updatePowerState()
        }
    }
    
    Process {
        id: scanProc
        running: false
        
        onExited: {
            if (exitCode !== 0) {
                root.scanning = false
            }
        }
    }
    
    Process {
        id: getDevices
        command: ["bluetoothctl", "devices"]
        running: false
        
        stdout: StdioCollector {
            onStreamFinished: {
                var output = this.text.trim()
                if (output.length === 0) return
                
                var lines = output.split("\n")
                var newDevices = []
                
                for (var i = 0; i < lines.length; i++) {
                    var line = lines[i]
                    var match = line.match(/Device ([0-9A-F:]+) (.+)/)
                    if (match) {
                        newDevices.push({
                            address: match[1],
                            name: match[2]
                        })
                    }
                }
                
                root.deviceQueue = newDevices
                root.processNextDevice()
            }
        }
    }
    
    Process {
        id: getDeviceInfo
        property string deviceAddress: ""
        property string deviceName: ""
        
        command: ["bluetoothctl", "info", deviceAddress]
        running: false
        
        stdout: StdioCollector {
            onStreamFinished: {
                var output = this.text
                var connected = output.includes("Connected: yes")
                var paired = output.includes("Paired: yes")

                var status = connected ? "Connected" : (paired ? "Paired" : "Available")

                var existingIndex = -1
                for (var i = 0; i < root.devices.length; i++) {
                    if (root.devices[i].address === getDeviceInfo.deviceAddress) {
                        existingIndex = i
                        break
                    }
                }

                if (existingIndex >= 0) {
                    root.devices[existingIndex].status = status
                    root.devices[existingIndex].connected = connected
                    root.devices[existingIndex].paired = paired
                } else {
                    root.devices.push(deviceComp.createObject(root, {
                        address: getDeviceInfo.deviceAddress,
                        name: getDeviceInfo.deviceName,
                        status: status,
                        connected: connected,
                        paired: paired
                    }))
                }
            }
        }

        onExited: root.processNextDevice()
    }
    
    Process {
        id: connectProc
        running: false
        
        onExited: {
            updateDevices()
        }
    }
    
    Process {
        id: disconnectProc
        running: false
        
        onExited: {
            updateDevices()
        }
    }
    
    Process {
        id: pairProc
        running: false
        
        onExited: {
            updateDevices()
        }
    }
    
    Timer {
        interval: 5000
        running: true
        repeat: true
        onTriggered: {
            root.updatePowerState()
            root.updateDevices()
        }
    }
    
    component BluetoothDevice: QtObject {
        required property string address
        required property string name
        property string status: "Available"
        property bool connected: false
        property bool paired: false
    }
    
    Component {
        id: deviceComp
        BluetoothDevice {}
    }
}
