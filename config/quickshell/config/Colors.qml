pragma Singleton

import QtQuick

QtObject {
    // bgs
    readonly property string background: "#0f0f0f"
    readonly property string surface: "#1a1a1a"
    readonly property string surfaceActive: "#242424"
    readonly property string subtle: "#343434"
    readonly property string border: "#2a2a2a"
    
    // text
    readonly property string text: "#cacaca"
    readonly property string textMuted: "#888888"
    readonly property string textBright: "#e1e1e1"
    readonly property string textDimmed: "#afafaf"


    // colors
    readonly property string red: "#b46958"
    readonly property string green: "#90A959"
    readonly property string blue: "#BAD7FF"
    readonly property string yellow: "#F4BF75"
    readonly property string magenta: "#AA759F"
    readonly property string orange: "#FFA557"
    readonly property string cyan: "#88afa2"
    

    
    // battery
    readonly property string batteryCharging: green
    readonly property string batteryNotCharging: cyan
    readonly property string batteryDischarging: yellow
    
    // power
    readonly property string profileEco: green
    readonly property string profileBalance: blue
    readonly property string profilePower: orange
    
    // workspaces
    readonly property string workspaceActive: "#282828"
    readonly property string workspaceInactive: surface
    readonly property string workspaceTextActive: "#fafafa"
    readonly property string workspaceTextInactive: "#aaaaaa"
}
