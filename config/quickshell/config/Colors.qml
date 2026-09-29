pragma Singleton

import QtQuick
import Quickshell

Singleton {
    id: root

    property string preview: ""
    property bool instant: false
    property string previewWallpaper: ""

    readonly property string themeId: preview.length > 0 ? preview : Settings.theme
    readonly property var base: Themes.find(themeId)
    readonly property var theme: base.dynamic ? dynamic : base

    readonly property var dynamic: {
        const fallback = Themes.find("wallpaper");
        const seed = pickSeed(quantizer.colors);
        if (!seed)
            return fallback;
        const h = seed.hslHue;
        const s = Math.max(0.45, Math.min(0.75, seed.hslSaturation));
        return Object.assign({}, fallback, Themes.neutrals(h), {
            primary: Qt.hsla(h, s, 0.78, 1),
            blue: Qt.hsla(h, s, 0.78, 1)
        });
    }

    function pickSeed(colors: var): var {
        let best = null;
        let bestScore = -1;
        for (const c of colors) {
            if (c.hsvValue < 0.2)
                continue;
            const score = c.hsvSaturation * 0.8 + c.hsvValue * 0.2;
            if (score > bestScore) {
                bestScore = score;
                best = c;
            }
        }
        return best && best.hsvSaturation > 0.12 ? best : null;
    }

    function mix(a: color, b: color, t: real): color {
        return Qt.tint(a, Qt.alpha(b, t));
    }

    function paletteOf(id: string): var {
        const t = Themes.find(id).dynamic ? dynamic : Themes.find(id);
        return {
            bg: t.bg,
            surface: mix(t.bg, t.fg, 0.07),
            subtle: mix(t.bg, t.fg, 0.16),
            muted: mix(t.bg, t.fg, 0.6),
            fg: t.fg,
            primary: t.primary,
            accents: [t.red, t.yellow, t.green, t.cyan, t.magenta]
        };
    }

    ColorQuantizer {
        id: quantizer
        source: "file://" + (root.base.dynamic && root.previewWallpaper.length > 0 ? root.previewWallpaper : Settings.wallpaper)
        depth: 3
        rescaleSize: 96
    }

    property color background: theme.bg
    property color backgroundDeep: mix(theme.bg, "#000000", 0.6)
    property color surface: mix(theme.bg, theme.fg, 0.055)
    property color surfaceActive: mix(theme.bg, theme.fg, 0.09)
    property color subtle: mix(theme.bg, theme.fg, 0.16)
    property color border: mix(theme.bg, theme.fg, 0.12)
    property color outline: mix(theme.bg, theme.fg, 0.22)

    property color text: theme.fg
    property color textMuted: mix(theme.bg, theme.fg, 0.6)
    property color textBright: mix(theme.fg, "#ffffff", 0.5)
    property color textDimmed: mix(theme.bg, theme.fg, 0.84)

    property color primary: theme.primary
    property color primaryText: mix(theme.primary, "#000000", 0.74)
    property color primaryContainer: mix(theme.bg, theme.primary, 0.2)
    property color primaryContainerText: mix(theme.primary, "#ffffff", 0.45)

    property color red: theme.red
    property color green: theme.green
    property color blue: theme.blue
    property color yellow: theme.yellow
    property color magenta: theme.magenta
    property color orange: theme.orange
    property color cyan: theme.cyan

    readonly property color batteryCharging: green
    readonly property color batteryNotCharging: cyan
    readonly property color batteryDischarging: yellow

    readonly property color profileEco: green
    readonly property color profileBalance: blue
    readonly property color profilePower: orange

    readonly property color workspaceActive: subtle
    readonly property color workspaceInactive: surface
    readonly property color workspaceTextActive: textBright
    readonly property color workspaceTextInactive: textMuted

    Behavior on background { Fade {} }
    Behavior on backgroundDeep { Fade {} }
    Behavior on surface { Fade {} }
    Behavior on surfaceActive { Fade {} }
    Behavior on subtle { Fade {} }
    Behavior on border { Fade {} }
    Behavior on outline { Fade {} }
    Behavior on text { Fade {} }
    Behavior on textMuted { Fade {} }
    Behavior on textBright { Fade {} }
    Behavior on textDimmed { Fade {} }
    Behavior on primary { Fade {} }
    Behavior on primaryText { Fade {} }
    Behavior on primaryContainer { Fade {} }
    Behavior on primaryContainerText { Fade {} }
    Behavior on red { Fade {} }
    Behavior on green { Fade {} }
    Behavior on blue { Fade {} }
    Behavior on yellow { Fade {} }
    Behavior on magenta { Fade {} }
    Behavior on orange { Fade {} }
    Behavior on cyan { Fade {} }

    component Fade: ColorAnimation {
        duration: root.instant ? 0 : 320
        easing.type: Easing.OutCubic
    }
}
