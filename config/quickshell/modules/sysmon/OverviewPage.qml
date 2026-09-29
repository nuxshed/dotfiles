import QtQuick
import QtQuick.Layouts
import "../../config"
import "../../services"

ColumnLayout {
    id: root

    spacing: 12

    RowLayout {
        Layout.fillWidth: true
        Layout.fillHeight: false
        spacing: 28

        Text {
            Layout.fillWidth: true
            text: "System Overview"
            color: Colors.textBright
            font.pixelSize: 18
            font.family: Fonts.family
            font.weight: Font.Medium
        }

        Stat { label: "Uptime"; value: SysMon.fmtDuration(SysMon.uptime) }
        Stat { label: "Load average"; value: SysMon.load }
        Stat { label: "Processes"; value: `${SysMon.procCount} (${SysMon.threadCount} threads)` }
    }

    GridLayout {
        Layout.fillWidth: true
        Layout.fillHeight: true
        columns: 2
        rowSpacing: 12
        columnSpacing: 12

        Card {
            Layout.fillWidth: true
            Layout.fillHeight: true
            title: "CPU"
            accent: Colors.primary
            subtitle: (SysMon.cpuTemp > 0 ? `${SysMon.cpuTemp.toFixed(0)}°C · ` : "") + `${(SysMon.freq / 1000).toFixed(2)} GHz`
            value: `${SysMon.cpu.toFixed(0)}%`
            series: [{ data: SysMon.cpuHist, color: Colors.primary }]
            clickable: true
            onClicked: SysMon.perfTab = "cpu"
        }

        Card {
            Layout.fillWidth: true
            Layout.fillHeight: true
            title: "GPU"
            accent: Colors.magenta
            subtitle: SysMon.gpuState === "active" ? `${SysMon.gpuTemp.toFixed(0)}°C · ${SysMon.gpuPower.toFixed(0)} W`
                : SysMon.gpuState === "suspended" ? "Discrete GPU asleep" : "No discrete GPU"
            value: SysMon.gpuState === "active" ? `${SysMon.gpuUtil.toFixed(0)}%` : "—"
            series: [{ data: SysMon.gpuHist, color: Colors.magenta }]
            clickable: true
            onClicked: SysMon.perfTab = "gpu"
        }

        Card {
            Layout.fillWidth: true
            Layout.fillHeight: true
            title: "Memory"
            accent: Colors.cyan
            subtitle: `${SysMon.fmtBytes(SysMon.memUsed, 1)} of ${SysMon.fmtBytes(SysMon.memTotal, 1)}`
            value: `${SysMon.memPct.toFixed(0)}%`
            series: [{ data: SysMon.memHist, color: Colors.cyan }]
            fmt: v => SysMon.fmtBytes(v / 100 * SysMon.memTotal, 1)
            clickable: true
            onClicked: SysMon.perfTab = "memory"
        }

        Card {
            Layout.fillWidth: true
            Layout.fillHeight: true
            title: "Network"
            accent: Colors.green
            subtitle: `↓ ${SysMon.fmtRate(SysMon.netRx)} · ↑ ${SysMon.fmtRate(SysMon.netTx)}`
            value: SysMon.fmtRate(SysMon.netRx + SysMon.netTx)
            series: [{ data: SysMon.netRxHist, color: Colors.green }, { data: SysMon.netTxHist, color: Colors.yellow }]
            max: 0
            fmt: v => SysMon.fmtRate(v)
            clickable: true
            onClicked: SysMon.perfTab = "network"
        }

        Card {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.columnSpan: 2
            title: "Disk"
            accent: Colors.red
            subtitle: `Read ${SysMon.fmtRate(SysMon.diskRead)} · Write ${SysMon.fmtRate(SysMon.diskWrite)}`
            value: SysMon.fmtRate(SysMon.diskRead + SysMon.diskWrite)
            series: [{ data: SysMon.diskReadHist, color: Colors.red }, { data: SysMon.diskWriteHist, color: Colors.orange }]
            max: 0
            fmt: v => SysMon.fmtRate(v)
            clickable: true
            onClicked: SysMon.perfTab = "disk"
        }
    }
}
