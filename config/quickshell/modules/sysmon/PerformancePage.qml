pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../config"
import "../../services"

ColumnLayout {
    id: root

    readonly property var tabs: [
        { id: "overview", icon: "dashboard", name: "Overview" },
        { id: "cpu", icon: "memory", name: "CPU" },
        { id: "gpu", icon: "videogame_asset", name: "GPU" },
        { id: "memory", icon: "storage", name: "Memory" },
        { id: "network", icon: "wifi", name: "Network" },
        { id: "disk", icon: "album", name: "Disk" }
    ]

    spacing: 16

    Tabs {
        Layout.fillWidth: true
        Layout.fillHeight: false
        items: root.tabs
        currentIndex: Math.max(0, root.tabs.findIndex(t => t.id === SysMon.perfTab))
        onSelected: i => SysMon.perfTab = root.tabs[i].id
    }

    Loader {
        Layout.fillWidth: true
        Layout.fillHeight: true
        sourceComponent: {
            switch (SysMon.perfTab) {
            case "cpu": return cpuPage;
            case "gpu": return gpuPage;
            case "memory": return memPage;
            case "network": return netPage;
            case "disk": return diskPage;
            default: return overviewPage;
            }
        }
    }

    Component {
        id: overviewPage
        OverviewPage {}
    }

    Component {
        id: cpuPage

        DetailPage {
            id: cpuDetail
            readonly property bool temp: mode === 1
            title: "CPU"
            subtitle: `${SysMon.cpuModel.length > 0 ? SysMon.cpuModel + " · " : ""}${SysMon.cpuCount} logical cores`
            value: temp ? `${SysMon.cpuTemp.toFixed(0)}°C` : `${SysMon.cpu.toFixed(0)}%`
            valueLabel: temp ? "package temperature" : "utilisation"
            modes: ["Usage", "Temperature"]
            fmt: temp ? (v => `${v.toFixed(0)}°C`) : null
            series: temp ? [{ data: SysMon.cpuTempHist, color: Colors.orange }] : [{ data: SysMon.cpuHist, color: Colors.blue }]
            extraTitle: "Cores"
            stats: [
                { label: "Frequency", value: `${(SysMon.freq / 1000).toFixed(2)} GHz` },
                { label: "Temperature", value: SysMon.cpuTemp > 0 ? `${SysMon.cpuTemp.toFixed(0)}°C` : "—" },
                { label: "Load average", value: SysMon.load },
                { label: "Uptime", value: SysMon.fmtDuration(SysMon.uptime) },
                { label: "Processes", value: String(SysMon.procCount) },
                { label: "Threads", value: String(SysMon.threadCount) }
            ]

            GridLayout {
                Layout.fillWidth: true
                columns: 2
                rowSpacing: 6
                columnSpacing: 16

                Repeater {
                    model: SysMon.cores

                    RowLayout {
                        required property int index
                        required property var modelData
                        Layout.fillWidth: true
                        spacing: 8

                        Text {
                            Layout.preferredWidth: 18
                            text: index
                            color: Colors.textMuted
                            font.pixelSize: 10
                            font.family: Fonts.family
                            horizontalAlignment: Text.AlignRight
                        }
                        Bar {
                            Layout.fillWidth: true
                            value: modelData / 100
                        }
                        Text {
                            Layout.preferredWidth: 30
                            text: `${modelData.toFixed(0)}%`
                            color: Colors.textDimmed
                            font.pixelSize: 10
                            font.family: Fonts.family
                            horizontalAlignment: Text.AlignRight
                        }
                    }
                }
            }
        }
    }

    Component {
        id: gpuPage

        DetailPage {
            readonly property bool active: SysMon.gpuState === "active"
            readonly property bool temp: mode === 1
            title: "GPU"
            subtitle: SysMon.gpuState === "none" ? "No discrete GPU detected"
                : active ? SysMon.gpuName : "Discrete GPU is asleep — it wakes automatically when used"
            value: !active ? "—" : temp ? `${SysMon.gpuTemp.toFixed(0)}°C` : `${SysMon.gpuUtil.toFixed(0)}%`
            valueLabel: active ? (temp ? "temperature" : "utilisation") : SysMon.gpuState
            modes: ["Usage", "Temperature"]
            fmt: temp ? (v => `${v.toFixed(0)}°C`) : null
            series: temp ? [{ data: SysMon.gpuTempHist, color: Colors.orange }] : [{ data: SysMon.gpuHist, color: Colors.magenta }]
            stats: [
                { label: "Temperature", value: active ? `${SysMon.gpuTemp.toFixed(0)}°C` : "—" },
                { label: "Power draw", value: active ? `${SysMon.gpuPower.toFixed(1)} W` : "—" },
                { label: "VRAM", value: active ? `${SysMon.fmtBytes(SysMon.gpuMemUsed, 1)} / ${SysMon.fmtBytes(SysMon.gpuMemTotal, 1)}` : "—" },
                { label: "Integrated GPU clock", value: SysMon.igpuMax > 0 ? `${SysMon.igpuFreq} / ${SysMon.igpuMax} MHz` : "—" }
            ]
            extraTitle: "GPU processes"

            Text {
                visible: SysMon.gpuProcs.length === 0
                text: active ? "Nothing is using the GPU" : "Discrete GPU is asleep"
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }

            Repeater {
                model: SysMon.gpuProcs

                KeyValue {
                    required property var modelData
                    label: `${modelData.name}  ·  ${modelData.pid}`
                    value: SysMon.fmtBytes(modelData.mem, 0)
                }
            }
        }
    }

    Component {
        id: memPage

        DetailPage {
            title: "Memory"
            subtitle: `${SysMon.fmtBytes(SysMon.memTotal, 1)} total`
            value: `${SysMon.memPct.toFixed(0)}%`
            valueLabel: `${SysMon.fmtBytes(SysMon.memUsed, 1)} used`
            series: [{ data: SysMon.memHist, color: Colors.cyan }]
            fmt: v => SysMon.fmtBytes(v / 100 * SysMon.memTotal, 1)
            extraTitle: "Usage"
            stats: [
                { label: "In use", value: SysMon.fmtBytes(SysMon.memUsed, 2) },
                { label: "Available", value: SysMon.fmtBytes(SysMon.memAvail, 2) },
                { label: "Cached", value: SysMon.fmtBytes(SysMon.memCached, 2) },
                { label: "Swap", value: SysMon.swapTotal > 0 ? `${SysMon.fmtBytes(SysMon.swapTotal - SysMon.swapFree, 1)} / ${SysMon.fmtBytes(SysMon.swapTotal, 1)}` : "None" }
            ]

            KeyValue { label: "Memory"; value: `${SysMon.memPct.toFixed(0)}%` }
            Bar { Layout.fillWidth: true; value: SysMon.memPct / 100; accent: Colors.cyan }
            KeyValue { Layout.topMargin: 6; label: "Swap"; value: SysMon.swapTotal > 0 ? `${((SysMon.swapTotal - SysMon.swapFree) / SysMon.swapTotal * 100).toFixed(0)}%` : "—" }
            Bar { Layout.fillWidth: true; value: SysMon.swapTotal > 0 ? (SysMon.swapTotal - SysMon.swapFree) / SysMon.swapTotal : 0; accent: Colors.textMuted }
        }
    }

    Component {
        id: netPage

        DetailPage {
            title: "Network"
            subtitle: SysMon.netIface.length > 0 ? SysMon.netIface : "No interface"
            value: SysMon.fmtRate(SysMon.netRx + SysMon.netTx)
            valueLabel: "throughput"
            series: [{ data: SysMon.netRxHist, color: Colors.green }, { data: SysMon.netTxHist, color: Colors.yellow }]
            max: 0
            legend: [{ name: "Download", color: Colors.green }, { name: "Upload", color: Colors.yellow }]
            fmt: v => SysMon.fmtRate(v)
            extraTitle: "Session totals"
            stats: [
                { label: "Download", value: SysMon.fmtRate(SysMon.netRx) },
                { label: "Upload", value: SysMon.fmtRate(SysMon.netTx) },
                { label: "Interface", value: SysMon.netIface }
            ]

            KeyValue { label: "Received"; value: SysMon.fmtBytes(SysMon.netRxTotal, 2) }
            KeyValue { label: "Sent"; value: SysMon.fmtBytes(SysMon.netTxTotal, 2) }
        }
    }

    Component {
        id: diskPage

        DetailPage {
            title: "Disk"
            subtitle: "Block devices"
            value: SysMon.fmtRate(SysMon.diskRead + SysMon.diskWrite)
            valueLabel: "throughput"
            series: [{ data: SysMon.diskReadHist, color: Colors.red }, { data: SysMon.diskWriteHist, color: Colors.orange }]
            max: 0
            legend: [{ name: "Read", color: Colors.red }, { name: "Write", color: Colors.orange }]
            fmt: v => SysMon.fmtRate(v)
            extraTitle: "Root filesystem"
            stats: [
                { label: "Read", value: SysMon.fmtRate(SysMon.diskRead) },
                { label: "Write", value: SysMon.fmtRate(SysMon.diskWrite) },
                { label: "Total read", value: SysMon.fmtBytes(SysMon.diskReadTotal, 1) },
                { label: "Total written", value: SysMon.fmtBytes(SysMon.diskWriteTotal, 1) }
            ]

            KeyValue { label: "Used"; value: `${SysMon.fmtBytes(SysMon.rootUsed, 1)} of ${SysMon.fmtBytes(SysMon.rootSize, 1)}` }
            Bar { Layout.fillWidth: true; value: SysMon.rootSize > 0 ? SysMon.rootUsed / SysMon.rootSize : 0; accent: Colors.red }
            KeyValue { label: "Free"; value: SysMon.fmtBytes(SysMon.rootSize - SysMon.rootUsed, 1) }
        }
    }
}
