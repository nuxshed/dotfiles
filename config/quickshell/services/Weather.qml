pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../config"

Singleton {
    id: root

    property bool ready: false
    property int temp: 0
    property int high: 0
    property int low: 0
    property int code: 0
    property string desc: ""
    property var hours: []

    property bool night: false

    function iconFor(code: int, night: bool): string {
        if (code === 113)
            return night ? "brightness_3" : "wb_sunny";
        if (code === 116)
            return night ? "cloud_queue" : "filter_drama";
        if (code === 119 || code === 122)
            return "cloud";
        if ([143, 248, 260].includes(code))
            return "blur_on";
        if ([200, 386, 389, 392, 395].includes(code))
            return "flash_on";
        if ([179, 182, 185, 227, 230, 317, 320, 323, 326, 329, 332, 335, 338, 350, 362, 365, 368, 371, 374, 377].includes(code))
            return "ac_unit";
        return "grain";
    }

    readonly property string icon: iconFor(code, night)

    function refresh(): void {
        fetch.running = false;
        fetch.command = ["curl", "-fsS", "--max-time", "15", "https://wttr.in/" + encodeURIComponent(SpotlightConfig.weatherLocation) + "?format=j1"];
        fetch.running = true;
    }

    function accept(text: string): void {
        let d;
        try {
            d = JSON.parse(text);
        } catch (e) {
            return;
        }
        const c = d.current_condition?.[0];
        const today = d.weather?.[0];
        if (!c || !today)
            return;

        const hour = new Date().getHours();
        root.night = hour < 6 || hour >= 19;
        root.temp = parseInt(c.temp_C);
        root.code = parseInt(c.weatherCode);
        root.desc = (c.weatherDesc?.[0]?.value ?? "").trim();
        root.high = parseInt(today.maxtempC);
        root.low = parseInt(today.mintempC);

        const now = new Date().getHours() * 100;
        const all = [];
        (d.weather ?? []).slice(0, 2).forEach((day, i) => {
            for (const h of day.hourly ?? [])
                all.push({ at: i * 2400 + parseInt(h.time), temp: parseInt(h.tempC), code: parseInt(h.weatherCode) });
        });
        root.hours = all.filter(h => h.at > now).slice(0, 4);
        root.ready = true;
    }

    Process {
        id: fetch

        stdout: StdioCollector {
            onStreamFinished: root.accept(text)
        }
    }

    Timer {
        interval: 30 * 60 * 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }
}
