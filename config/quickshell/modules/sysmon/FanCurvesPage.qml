pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"
import "../files"

ColumnLayout {
    id: root

    readonly property var profiles: ["Quiet", "Balanced", "Performance"]
    property string profile: Asusctl.activeProfile
    property string fan: "cpu"
    property var draft: []
    property bool dirty: false
    readonly property var current: SysMon.profileCurves.find(c => c.id === root.fan) ?? null
    readonly property var liveFan: SysMon.fans.find(f => f.label.toLowerCase() === root.fan) ?? null

    function load(): void {
        root.draft = root.current ? root.current.points.map(p => ({ t: p.t, pwm: p.pwm })) : [];
        root.dirty = false;
    }

    onProfileChanged: SysMon.loadCurves(root.profile)
    onFanChanged: root.load()
    onCurrentChanged: if (!root.dirty) root.load()

    Component.onCompleted: SysMon.loadCurves(root.profile)

    spacing: 12

    RowLayout {
        Layout.fillWidth: true
        Layout.fillHeight: false
        spacing: 12

        PageHeader {
            title: "Fan curves"
            subtitle: root.profile === Asusctl.activeProfile ? `Editing the active ${root.profile} profile` : `Editing ${root.profile} · ${Asusctl.activeProfile} is active`
        }

        Tabs {
            Layout.preferredWidth: 330
            items: root.profiles.map(p => ({ name: p, icon: p === "Quiet" ? "spa" : p === "Balanced" ? "tune" : "flash_on" }))
            currentIndex: Math.max(0, root.profiles.indexOf(root.profile))
            onSelected: i => { root.profile = root.profiles[i]; root.dirty = false; }
        }
    }

    RowLayout {
        Layout.fillWidth: true
        Layout.fillHeight: true
        spacing: 12

        ColumnLayout {
            Layout.preferredWidth: 220
            Layout.fillWidth: false
            Layout.fillHeight: true
            spacing: 8

            Repeater {
                model: SysMon.profileCurves

                Rectangle {
                    id: fanCard
                    required property var modelData
                    readonly property bool active: root.fan === modelData.id
                    readonly property var rpm: SysMon.fans.find(f => f.label.toLowerCase() === modelData.id) ?? null
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    radius: 10
                    color: active ? Colors.subtle : cardMouse.containsMouse ? Colors.surfaceActive : "transparent"
                    border.width: 1
                    border.color: active ? Colors.subtle : Colors.border

                    Behavior on color { ColorAnimation { duration: 120 } }

                    ColumnLayout {
                        anchors.fill: parent
                        anchors.margins: 12
                        spacing: 4

                        RowLayout {
                            Layout.fillWidth: true

                            Text {
                                Layout.fillWidth: true
                                text: `${fanCard.modelData.name} fan`
                                color: fanCard.active ? Colors.textBright : Colors.text
                                font.pixelSize: 12
                                font.family: Fonts.family
                                font.weight: Font.Medium
                            }
                            Rectangle {
                                width: 7
                                height: 7
                                radius: 4
                                color: fanCard.modelData.enabled ? Colors.green : Colors.textMuted
                            }
                        }
                        Text {
                            text: (fanCard.rpm ? `${fanCard.rpm.rpm} rpm · ` : "") + (fanCard.modelData.enabled ? "custom curve" : "firmware curve")
                            color: Colors.textMuted
                            font.pixelSize: 10
                            font.family: Fonts.family
                        }

                        Canvas {
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            Layout.minimumHeight: 40
                            readonly property var pts: fanCard.modelData.points
                            onPtsChanged: requestPaint()
                            onWidthChanged: requestPaint()
                            onHeightChanged: requestPaint()

                            onPaint: {
                                const ctx = getContext("2d");
                                ctx.clearRect(0, 0, width, height);
                                if (pts.length === 0)
                                    return;
                                const px = t => (t - 30) / 70 * (width - 2) + 1;
                                const py = p => height - 1 - p / 255 * (height - 2);
                                ctx.beginPath();
                                ctx.moveTo(1, py(pts[0].pwm));
                                for (const p of pts)
                                    ctx.lineTo(px(p.t), py(p.pwm));
                                ctx.lineTo(width - 1, py(pts[pts.length - 1].pwm));
                                ctx.strokeStyle = fanCard.modelData.enabled ? Colors.primary : Colors.textDimmed;
                                ctx.lineWidth = 1.5;
                                ctx.lineJoin = "round";
                                ctx.stroke();
                            }
                        }
                    }

                    MouseArea {
                        id: cardMouse
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: root.fan = fanCard.modelData.id
                    }
                }
            }

            Text {
                visible: SysMon.profileCurves.length === 0
                text: SysMon.curveBusy ? "Loading…" : "asusctl reported no fan curves"
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            radius: 10
            color: Colors.surfaceActive
            border.width: 1
            border.color: Colors.outline

            ColumnLayout {
                anchors.fill: parent
                anchors.margins: 14
                spacing: 10

                RowLayout {
                    Layout.fillWidth: true
                    Layout.fillHeight: false
                    spacing: 10

                    Column {
                        Layout.fillWidth: true
                        spacing: 2

                        Text {
                            text: root.current ? `${root.current.name} fan` : "Fan"
                            color: Colors.textBright
                            font.pixelSize: 13
                            font.family: Fonts.family
                            font.weight: Font.Medium
                        }
                        Text {
                            text: root.current && root.current.enabled ? "Custom curve enabled — drag the points to edit" : "Firmware curve — drag points and apply to switch to a custom curve"
                            color: Colors.textMuted
                            font.pixelSize: 10
                            font.family: Fonts.family
                        }
                    }

                    Text {
                        text: "Custom"
                        color: Colors.textMuted
                        font.pixelSize: 11
                        font.family: Fonts.family
                    }
                    Toggle {
                        checked: root.current ? root.current.enabled : false
                        enabled: root.current !== null && !SysMon.curveBusy
                        onToggled: SysMon.setCurveEnabled(root.profile, root.fan, !root.current.enabled)
                    }
                }

                FanCurveEditor {
                    id: editor
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    points: root.draft
                    enabled: root.current !== null && !SysMon.curveBusy
                    opacity: root.current && root.current.enabled ? 1 : 0.55
                    onChanged: {
                        root.draft = editor.points;
                        root.dirty = true;
                    }
                }

                RowLayout {
                    Layout.fillWidth: true
                    Layout.fillHeight: false
                    spacing: 6

                    Repeater {
                        model: root.draft

                        Column {
                            required property var modelData
                            Layout.fillWidth: true
                            spacing: 1

                            Text {
                                anchors.horizontalCenter: parent.horizontalCenter
                                text: `${modelData.t}°`
                                color: Colors.textDimmed
                                font.pixelSize: 10
                                font.family: Fonts.family
                            }
                            Text {
                                anchors.horizontalCenter: parent.horizontalCenter
                                text: `${Math.round(modelData.pwm / 255 * 100)}%`
                                color: Colors.textBright
                                font.pixelSize: 11
                                font.family: Fonts.family
                                font.weight: Font.Medium
                            }
                        }
                    }
                }

                RowLayout {
                    Layout.fillWidth: true
                    Layout.fillHeight: false
                    spacing: 6

                    Text {
                        Layout.fillWidth: true
                        text: SysMon.curveBusy ? "Applying…" : root.dirty ? "Unsaved changes" : ""
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }

                    TextButton {
                        text: "Reset profile"
                        implicitHeight: 30
                        enabled: !SysMon.curveBusy
                        onClicked: { root.dirty = false; SysMon.resetCurves(root.profile); }
                    }
                    TextButton {
                        text: "Revert"
                        implicitHeight: 30
                        enabled: root.dirty
                        onClicked: root.load()
                    }
                    TextButton {
                        text: "Apply"
                        implicitHeight: 30
                        primary: true
                        enabled: root.dirty && !SysMon.curveBusy
                        onClicked: { root.dirty = false; SysMon.applyCurve(root.profile, root.fan, root.draft, true); }
                    }
                }
            }
        }
    }
}
