pragma Singleton

import QtQuick

QtObject {
    readonly property var base: ({
        bg: "#101114", fg: "#d0d3d9",
        red: "#b46958", green: "#90a959", yellow: "#f4bf75", blue: "#a8ccff", magenta: "#aa759f", orange: "#ffa557", cyan: "#88afa2"
    })

    readonly property var list: [
        tone("sky", "Sky", "#a8ccff"),
        tone("lavender", "Lavender", "#c4b5fd"),
        tone("sage", "Sage", "#a8d5a2"),
        tone("teal", "Teal", "#8fd5cc"),
        tone("rose", "Rose", "#f4a9c0"),
        tone("amber", "Amber", "#f2c57c"),
        tone("silver", "Silver", "#e4e6ea"),
        Object.assign(tone("wallpaper", "Wallpaper", "#a8ccff"), { dynamic: true })
    ]

    function neutrals(hue: real): var {
        return {
            bg: Qt.hsla(hue, 0.09, 0.066, 1),
            fg: Qt.hsla(hue, 0.07, 0.84, 1)
        };
    }

    function tone(id: string, name: string, primary: string): var {
        const c = Qt.tint(primary, "transparent");
        const tinted = c.hslSaturation > 0.1 ? neutrals(c.hslHue) : {};
        return Object.assign({}, base, tinted, { id: id, name: name, primary: primary });
    }

    function find(id: string): var {
        return list.find(t => t.id === id) ?? list[0];
    }
}
