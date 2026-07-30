pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io

/**
 * Network service that provides WiFi connection status and signal strength.
 */
Singleton {
    id: root
    
    property int signalStrength: 0  // 0-100 (Wi-Fi only)
    property int signalLevel: 0     // 0-4 bars (Wi-Fi only)
    property bool isConnected: false
    property bool isWiredConnected: false
    property string ssid: ""
    property bool isAvailable: false
    
    readonly property list<AccessPoint> networks: []
    readonly property AccessPoint active: networks.find(n => n.active) ?? null
    property bool wifiEnabled: true
    readonly property bool scanning: rescanProc.running
    property var savedConnections: []
    
    signal networkChanged()
    
    function updateNetwork() {
        getNetworks.running = true
    }
    
    function enableWifi(enabled) {
        const cmd = enabled ? "on" : "off"
        enableWifiProc.exec(["nmcli", "radio", "wifi", cmd])
    }
    
    function toggleWifi() {
        const cmd = wifiEnabled ? "off" : "on"
        enableWifiProc.exec(["nmcli", "radio", "wifi", cmd])
    }
    
    function rescanWifi() {
        rescanProc.running = true
    }
    
    function connectToNetwork(ssid, password) {
        if (password && password.length > 0) {
            // Connect with password
            connectProc.exec(["nmcli", "dev", "wifi", "connect", ssid, "password", password])
        } else {
            // Connect to open network or already saved network
            connectProc.exec(["nmcli", "conn", "up", ssid])
        }
    }
    
    function disconnectFromNetwork() {
        if (active) {
            disconnectProc.exec(["nmcli", "connection", "down", active.ssid])
        }
    }
    
    function getWifiStatus() {
        wifiStatusProc.running = true
    }
    
    function getSavedConnections() {
        savedConnectionsProc.running = true
    }
    
    Component.onCompleted: {
        updateNetwork()
        getSavedConnections()
        getDeviceStatus.running = true
    }
    
    // Monitor NetworkManager events
    Process {
        running: true
        command: ["nmcli", "m"]
        stdout: SplitParser {
            onRead: {
                getNetworks.running = true
                getDeviceStatus.running = true
            }
        }
    }

    Process {
        id: getDeviceStatus

        command: ["nmcli", "-g", "DEVICE,TYPE,STATE", "d"]
        environment: ({
            LANG: "C.UTF-8",
            LC_ALL: "C.UTF-8"
        })
        stdout: StdioCollector {
            onStreamFinished: {
                const lines = text.trim().split("\n").filter(l => l.length > 0)
                let wiredUp = false

                for (const line of lines) {
                    const parts = line.split(":")
                    const type = parts[1]
                    const state = parts[2]

                    if (type === "ethernet" && (state === "connected" || state === "connected (global)")) {
                        wiredUp = true
                        break
                    }
                }

                if (root.isWiredConnected !== wiredUp) {
                    root.isWiredConnected = wiredUp
                    root.networkChanged()
                }
            }
        }
    }
    
    Process {
        id: wifiStatusProc
        
        running: true
        command: ["nmcli", "radio", "wifi"]
        environment: ({
            LANG: "C.UTF-8",
            LC_ALL: "C.UTF-8"
        })
        stdout: StdioCollector {
            onStreamFinished: {
                root.wifiEnabled = text.trim() === "enabled"
            }
        }
    }
    
    Process {
        id: savedConnectionsProc
        
        running: true
        command: ["nmcli", "-g", "NAME,TYPE", "connection", "show"]
        environment: ({
            LANG: "C.UTF-8",
            LC_ALL: "C.UTF-8"
        })
        stdout: StdioCollector {
            onStreamFinished: {
                root.savedConnections = text.trim().split("\n")
                    .map(line => {
                        const parts = line.split(":")
                        return { name: parts[0], type: parts[1] }
                    })
                    .filter(conn => conn.type === "802-11-wireless")
                    .map(conn => conn.name)
                getNetworks.running = true
            }
        }
    }
    
    Process {
        id: enableWifiProc
        
        onExited: {
            root.getWifiStatus()
            getNetworks.running = true
        }
    }
    
    Process {
        id: rescanProc
        
        command: ["nmcli", "dev", "wifi", "list", "--rescan", "yes"]
        onExited: {
            getNetworks.running = true
        }
    }
    
    Process {
        id: connectProc
        
        stdout: SplitParser {
            onRead: getNetworks.running = true
        }
        stderr: StdioCollector {
            onStreamFinished: console.warn("Network connection error:", text)
        }
        onExited: {
            root.getSavedConnections()
        }
    }
    
    Process {
        id: disconnectProc
        
        stdout: SplitParser {
            onRead: getNetworks.running = true
        }
    }
    
    Process {
        id: getNetworks
        
        running: true
        command: ["nmcli", "-g", "ACTIVE,SIGNAL,FREQ,SSID,BSSID,SECURITY", "d", "w"]
        environment: ({
            LANG: "C.UTF-8",
            LC_ALL: "C.UTF-8"
        })
        stdout: StdioCollector {
            onStreamFinished: {
                const PLACEHOLDER = "STRINGWHICHHOPEFULLYWONTBEUSED"
                const rep = new RegExp("\\\\:", "g")
                const rep2 = new RegExp(PLACEHOLDER, "g")
                
                const allNetworks = text.trim().split("\n").map(n => {
                    const net = n.replace(rep, PLACEHOLDER).split(":")
                    return {
                        active: net[0] === "yes",
                        strength: parseInt(net[1]),
                        frequency: parseInt(net[2]),
                        ssid: net[3]?.replace(rep2, ":") ?? "",
                        bssid: net[4]?.replace(rep2, ":") ?? "",
                        security: net[5] ?? ""
                    }
                }).filter(n => n.ssid && n.ssid.length > 0)
                
                // Group networks by SSID and prioritize connected ones
                const networkMap = new Map()
                for (const network of allNetworks) {
                    const existing = networkMap.get(network.ssid)
                    if (!existing) {
                        networkMap.set(network.ssid, network)
                    } else {
                        // Prioritize active/connected networks
                        if (network.active && !existing.active) {
                            networkMap.set(network.ssid, network)
                        } else if (!network.active && !existing.active) {
                            // If both are inactive, keep the one with better signal
                            if (network.strength > existing.strength) {
                                networkMap.set(network.ssid, network)
                            }
                        }
                        // If existing is active and new is not, keep existing
                    }
                }
                
                const networks = Array.from(networkMap.values())
                
                const rNetworks = root.networks
                
                const destroyed = rNetworks.filter(rn => !networks.find(n => n.frequency === rn.frequency && n.ssid === rn.ssid && n.bssid === rn.bssid))
                for (const network of destroyed)
                    rNetworks.splice(rNetworks.indexOf(network), 1).forEach(n => n.destroy())
                
                for (const network of networks) {
                    const match = rNetworks.find(n => n.frequency === network.frequency && n.ssid === network.ssid && n.bssid === network.bssid)
                    if (match) {
                        match.lastIpcObject = network
                    } else {
                        rNetworks.push(apComp.createObject(root, {
                            lastIpcObject: network
                        }))
                    }
                }
                
                // Update convenience properties
                var activeConnection = networks.find(n => n.active)
                var newConnected = activeConnection !== null
                var newStrength = activeConnection ? activeConnection.strength : 0
                var newSsid = activeConnection ? activeConnection.ssid : ""
                
                // Convert signal strength (0-100) to level (0-4)
                var newLevel = 0
                if (newStrength > 0) {
                    if (newStrength <= 25) newLevel = 1
                    else if (newStrength <= 50) newLevel = 2
                    else if (newStrength <= 75) newLevel = 3
                    else newLevel = 4
                }
                
                var changed = (
                    newConnected !== root.isConnected ||
                    newStrength !== root.signalStrength ||
                    newSsid !== root.ssid
                )
                
                root.isConnected = newConnected
                root.signalStrength = newStrength
                root.signalLevel = newLevel
                root.ssid = newSsid
                root.isAvailable = true
                
                if (changed) {
                    root.networkChanged()
                }
            }
        }
        
        onExited: {
            if (exitCode !== 0) {
                root.isAvailable = false
                root.isConnected = false
                root.signalStrength = 0
                root.signalLevel = 0
                root.ssid = ""
            }
        }
    }
    
    component AccessPoint: QtObject {
        required property var lastIpcObject
        readonly property string ssid: lastIpcObject.ssid
        readonly property string bssid: lastIpcObject.bssid
        readonly property int strength: lastIpcObject.strength
        readonly property int frequency: lastIpcObject.frequency
        readonly property bool active: lastIpcObject.active
        readonly property string security: lastIpcObject.security
        readonly property bool isSecure: security.length > 0
        readonly property bool isSaved: root.savedConnections.includes(ssid)
    }
    
    Component {
        id: apComp
        
        AccessPoint {}
    }
}
