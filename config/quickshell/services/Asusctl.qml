pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io

/**
 * Asusctl service that provides power profile information and control.
 */
Singleton {
    id: root
    
    property string activeProfile: "Balanced"
    property string keyboardBrightness: "Off"
    property string auraMode: "static"
    property bool isAvailable: false
    
    readonly property list<string> auraModes: ["static", "breathe", "rainbow-cycle", "rainbow-wave", "pulse"]
    readonly property string auraColor: "623987" // Default purple color
    
    signal profileChanged()
    
    function getAuraDisplayName(mode) {
        if (mode === "rainbow-cycle") return "r-cycle"
        if (mode === "rainbow-wave") return "r-wave"
        return mode
    }
    
    function updateProfile() {
        profileProcess.running = true
    }
    
    function updateKeyboardBrightness() {
        keyboardProcess.running = true
    }
    
    function nextKeyboardBrightness() {
        nextKbdProcess.running = true
    }
    
    function prevKeyboardBrightness() {
        prevKbdProcess.running = true
    }
    
    function nextAuraMode() {
        nextAuraProcess.running = true
    }
    
    function prevAuraMode() {
        prevAuraProcess.running = true
    }
    
    function setProfile(profile) {
        var command = []
        if (profile === "Quiet") {
            command = ["asusctl", "profile", "set", "Quiet"]
        } else if (profile === "Balanced") {
            command = ["asusctl", "profile", "set", "Balanced"]
        } else if (profile === "Performance") {
            command = ["asusctl", "profile", "set", "Performance"]
        }
        
        if (command.length > 0) {
            root.activeProfile = profile
            root.profileChanged()
            setProfileProcess.command = command
            setProfileProcess.running = true
        }
    }
    
    Component.onCompleted: {
        updateProfile()
        updateKeyboardBrightness()
    }
    
    Process {
        id: profileProcess
        command: ["asusctl", "profile", "get"]
        running: false
        
        stdout: StdioCollector {
            onStreamFinished: {
                var output = this.text.trim()
                
                if (output.length === 0) {
                    root.isAvailable = false
                    return
                }
                
                var match = output.match(/Active profile(?: is|:)?\s*(\w+)/)
                if (match) {
                    var newProfile = match[1]
                    var changed = (newProfile !== root.activeProfile)
                    
                    root.activeProfile = newProfile
                    root.isAvailable = true
                    
                    if (changed) {
                        root.profileChanged()
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
    
    Process {
        id: setProfileProcess
        running: false
        
        onExited: {
            // After setting profile, update to get current state
            updateProfile()
        }
    }
    
    Process {
        id: keyboardProcess
        command: ["asusctl", "leds", "get"]
        running: false
        
        stdout: StdioCollector {
            onStreamFinished: {
                var output = this.text.trim()
                var match = output.match(/Current keyboard led brightness: (\w+)/)
                if (match) {
                    root.keyboardBrightness = match[1]
                }
            }
        }
    }
    
    Process {
        id: nextKbdProcess
        command: ["asusctl", "leds", "next"]
        running: false
        
        onExited: {
            updateKeyboardBrightness()
        }
    }
    
    Process {
        id: prevKbdProcess
        command: ["asusctl", "leds", "prev"]
        running: false
        
        onExited: {
            updateKeyboardBrightness()
        }
    }
    
    Process {
        id: nextAuraProcess
        command: ["asusctl", "aura", "effect", "--next-mode"]
        running: false
        
        onExited: {
            // Cycle to next mode
            var currentIndex = root.auraModes.indexOf(root.auraMode)
            var nextIndex = (currentIndex + 1) % root.auraModes.length
            var newMode = root.auraModes[nextIndex]
            root.auraMode = newMode
            
            // Apply color for modes that support it
            if (newMode === "static") {
                applyAuraProcess.command = ["asusctl", "aura", "static", "-c", root.auraColor]
                applyAuraProcess.running = true
            } else if (newMode === "breathe") {
                applyAuraProcess.command = ["asusctl", "aura", "breathe", "-c", root.auraColor]
                applyAuraProcess.running = true
            } else if (newMode === "pulse") {
                applyAuraProcess.command = ["asusctl", "aura", "pulse", "-c", root.auraColor]
                applyAuraProcess.running = true
            }
        }
    }
    
    Process {
        id: prevAuraProcess
        command: ["asusctl", "aura", "effect", "--prev-mode"]
        running: false
        
        onExited: {
            // Cycle to previous mode
            var currentIndex = root.auraModes.indexOf(root.auraMode)
            var prevIndex = (currentIndex - 1 + root.auraModes.length) % root.auraModes.length
            var newMode = root.auraModes[prevIndex]
            root.auraMode = newMode
            
            // Apply color for modes that support it
            if (newMode === "static") {
                applyAuraProcess.command = ["asusctl", "aura", "static", "-c", root.auraColor]
                applyAuraProcess.running = true
            } else if (newMode === "breathe") {
                applyAuraProcess.command = ["asusctl", "aura", "breathe", "-c", root.auraColor]
                applyAuraProcess.running = true
            } else if (newMode === "pulse") {
                applyAuraProcess.command = ["asusctl", "aura", "pulse", "-c", root.auraColor]
                applyAuraProcess.running = true
            }
        }
    }
    
    Process {
        id: applyAuraProcess
        running: false
    }
    
    Timer {
        interval: 10000 // Check every 10 seconds
        running: true
        repeat: true
        onTriggered: root.updateProfile()
    }
}
