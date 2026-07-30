pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io

/**
 * Battery service that provides battery level and charging status.
 */
Singleton {
    id: root
    
    property int level: 0
    property bool isCharging: false
    property string status: "Unknown"
    property string timeRemaining: ""
    property bool isAvailable: false
    
    signal batteryChanged()
    
    function updateBattery() {
        batteryProcess.running = true
    }
    
    Component.onCompleted: {
        updateBattery()
    }
    
    Process {
        id: batteryProcess
        command: ["acpi", "-b"]
        running: false
        
        stdout: StdioCollector {
            onStreamFinished: {
                var output = this.text.trim()
                
                if (output.length === 0) {
                    root.isAvailable = false
                    return
                }
                
                var match = output.match(/(\d+)%/)
                if (match) {
                    var newLevel = parseInt(match[1])
                    var newCharging = output.includes("Charging")
                    
                    // Extract status string (Charging, Discharging, Not charging, etc.)
                    var statusMatch = output.match(/Battery \d+: ([^,]+)/)
                    var newStatus = statusMatch ? statusMatch[1] : "Unknown"
                    
                    // Extract time remaining (e.g., "04:47:49 remaining")
                    var timeMatch = output.match(/(\d{2}:\d{2}:\d{2}) remaining/)
                    var newTime = timeMatch ? timeMatch[1].substring(0, 5) : "" // Get HH:MM only
                    
                    var changed = (newLevel !== root.level || newCharging !== root.isCharging || newStatus !== root.status || newTime !== root.timeRemaining)
                    
                    root.level = newLevel
                    root.isCharging = newCharging
                    root.status = newStatus
                    root.timeRemaining = newTime
                    root.isAvailable = true
                    
                    if (changed) {
                        root.batteryChanged()
                    }
                }
            }
        }
        
        onExited: {
            if (exitCode !== 0) {
                root.isAvailable = false
            }
        }
    }
    
    Timer {
        interval: 5000
        running: true
        repeat: true
        onTriggered: root.updateBattery()
    }
}
