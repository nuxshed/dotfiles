pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

ColumnLayout {
    id: root

    readonly property real hottest: SysMon.temps.reduce((a, t) => Math.max(a, t.value), 0)
    readonly property var profiles: ["Quiet", "Balanced", "Performance"]

    function tempColor(v: real): color {
        return v >= 85 ? Colors.red : v >= 70 ? Colors.yellow : Colors.primary;
    }

    spacing: 16

    Tabs {
        Layout.fillWidth: true
        Layout.fillHeight: false
        items: [{ name: "Overview", icon: "whatshot" }, { name: "Fan curves", icon: "tune" }]
        currentIndex: SysMon.sensorsTab
        onSelected: i => SysMon.sensorsTab = i
    }

    Loader {
        Layout.fillWidth: true
        Layout.fillHeight: true
        sourceComponent: SysMon.sensorsTab === 1 ? curvesPage : overview
    }

    Component {
        id: curvesPage
        FanCurvesPage {}
    }

    Component {
        id: overview

        ColumnLayout {
            spacing: 12

            PageHeader {
                title: "Sensors"
                subtitle: `${SysMon.temps.length} temperature sensors · ${SysMon.fans.length} fans · ${Asusctl.activeProfile} profile`
                value: root.hottest > 0 ? `${root.hottest.toFixed(0)}°C` : "—"
                valueLabel: "hottest sensor"
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: false
                Layout.preferredHeight: 200
                spacing: 12

                Gauge {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    label: "CPU"
                    sublabel: `${(SysMon.freq / 1000).toFixed(2)} GHz`
                    value: SysMon.cpuTemp
                    display: SysMon.cpuTemp.toFixed(0)
                    unit: "°C"
                    accent: root.tempColor(SysMon.cpuTemp)
                    available: SysMon.cpuTemp > 0
                }

                Gauge {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    label: "GPU"
                    sublabel: SysMon.gpuState === "active" ? `${SysMon.gpuPower.toFixed(0)} W` : SysMon.gpuState === "suspended" ? "asleep" : "none"
                    value: SysMon.gpuTemp
                    display: SysMon.gpuTemp.toFixed(0)
                    unit: "°C"
                    accent: root.tempColor(SysMon.gpuTemp)
                    available: SysMon.gpuState === "active"
                }

                Repeater {
                    model: SysMon.fans.filter(f => f.label !== "ACPI")

                    Gauge {
                        required property var modelData
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        label: `${modelData.label} fan`
                        sublabel: SysMon.curves.find(c => c.name.toUpperCase() === modelData.label)?.enabled ? "custom curve" : "firmware curve"
                        value: modelData.rpm
                        max: 6000
                        display: String(modelData.rpm)
                        unit: "rpm"
                        accent: Colors.textDimmed
                    }
                }
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: 12

                Panel {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    Layout.preferredWidth: 3
                    title: "Temperatures"

                    GridLayout {
                        Layout.fillWidth: true
                        columns: 2
                        rowSpacing: 10
                        columnSpacing: 24

                        Repeater {
                            model: SysMon.temps

                            RowLayout {
                                id: tRow
                                required property var modelData
                                Layout.fillWidth: true
                                Layout.preferredWidth: 1
                                spacing: 10

                                Text {
                                    Layout.preferredWidth: 105
                                    text: tRow.modelData.label !== tRow.modelData.chip ? `${tRow.modelData.chip} ${tRow.modelData.label}` : tRow.modelData.label
                                    color: Colors.text
                                    font.pixelSize: 11
                                    font.family: Fonts.family
                                    elide: Text.ElideRight
                                }
                                Bar {
                                    Layout.fillWidth: true
                                    value: tRow.modelData.value / 100
                                    accent: root.tempColor(tRow.modelData.value)
                                }
                                Text {
                                    Layout.preferredWidth: 40
                                    text: `${tRow.modelData.value.toFixed(0)}°C`
                                    color: Colors.textBright
                                    font.pixelSize: 11
                                    font.family: Fonts.family
                                    horizontalAlignment: Text.AlignRight
                                }
                            }
                        }
                    }
                }

                Panel {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    Layout.preferredWidth: 2
                    title: "Power profile"
                    visible: Asusctl.isAvailable

                    Segmented {
                        Layout.fillWidth: true
                        implicitHeight: 30
                        items: root.profiles
                        currentIndex: Math.max(0, root.profiles.indexOf(Asusctl.activeProfile))
                        onSelected: i => Asusctl.setProfile(root.profiles[i])
                    }

                    KeyValue { Layout.topMargin: 6; label: "CPU clock"; value: `${(SysMon.freq / 1000).toFixed(2)} GHz` }
                    KeyValue { label: "GPU power"; value: SysMon.gpuState === "active" ? `${SysMon.gpuPower.toFixed(1)} W` : "asleep" }
                    KeyValue { label: "Battery draw"; value: SysMon.batPower > 0 ? `${SysMon.batPower.toFixed(1)} W` : "—" }
                    KeyValue { label: "Fan curves"; value: SysMon.curves.some(c => c.enabled) ? "custom" : "firmware" }
                }
            }
        }
    }
}
