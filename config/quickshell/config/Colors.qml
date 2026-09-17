pragma Singleton

import QtQuick

QtObject {
    // bgs (material-you dark, cool grey)
    readonly property string background: "#101114"
    readonly property string backgroundDeep: "#000000"
    readonly property string surface: "#1b1c1f"
    readonly property string surfaceActive: "#212226"
    readonly property string subtle: "#2f3136"
    readonly property string border: "#2a2c31"
    readonly property string outline: "#3b3e44"

    // text
    readonly property string text: "#d0d3d9"
    readonly property string textMuted: "#8b9099"
    readonly property string textBright: "#e8eaef"
    readonly property string textDimmed: "#b3b7bf"

    // accent (light blue primary, teal container)
    readonly property string primary: "#a8ccff"
    readonly property string primaryText: "#0b2a44"
    readonly property string primaryContainer: "#124a5e"
    readonly property string primaryContainerText: "#c4e4ff"

    // colors
    readonly property string red: "#b46958"
    readonly property string green: "#90A959"
    readonly property string blue: "#a8ccff"
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
