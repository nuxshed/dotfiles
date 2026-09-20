pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import QtMultimedia
import Quickshell
import "../../components"
import "../../config"
import "../../services"

FloatingWindow {
    id: root

    readonly property var cam: camLoader.item?.cam ?? null
    readonly property size frameSize: cam && cam.cameraFormat.resolution.width > 0 ? cam.cameraFormat.resolution : Qt.size(1920, 1080)
    readonly property real aspect: frameSize.width / frameSize.height
    readonly property real fitW: Math.min(viewport.width, viewport.height * aspect)
    readonly property real fitH: fitW / aspect

    visible: Booth.open
    implicitWidth: 960
    implicitHeight: 660
    minimumSize.width: 480
    minimumSize.height: 360
    color: "transparent"
    title: "Photo Booth"

    onVisibleChanged: if (visible) keyScope.forceActiveFocus()

    WindowChrome { titleHeight: 52 }

    function bestFormat(cam: var): var {
        let best = null;
        for (const f of cam.cameraDevice.videoFormats) {
            if (f.maxFrameRate < 24)
                continue;
            const area = f.resolution.width * f.resolution.height;
            const bestArea = best ? best.resolution.width * best.resolution.height : 0;
            if (area > bestArea || (area === bestArea && f.maxFrameRate > best.maxFrameRate))
                best = f;
        }
        return best;
    }

    function capture(path: string): void {
        const w = root.frameSize.width;
        const h = root.frameSize.height;
        const tmp = `/tmp/qs-booth-${Date.now()}.ppm`;
        shot.grabToImage(result => {
            if (result.saveToFile(tmp))
                Booth.deliver(tmp, path, w, h);
        }, Qt.size(w, h));
    }

    Connections {
        target: Booth

        function onCaptureRequested(path: string): void {
            root.capture(path);
        }
    }

    Loader {
        id: camLoader
        active: Booth.open

        sourceComponent: Item {
            readonly property alias cam: cam

            Camera {
                id: cam
                active: true

                function pickFormat(): void {
                    const f = root.bestFormat(cam);
                    if (f)
                        cameraFormat = f;
                }

                Component.onCompleted: pickFormat()
                onCameraDeviceChanged: pickFormat()
            }

            CaptureSession {
                camera: cam
                videoOutput: vo
            }
        }
    }

    Item {
        id: keyScope
        anchors.fill: parent
        focus: true

        Keys.onPressed: event => {
            if (event.key === Qt.Key_Escape) {
                if (Booth.picking)
                    Booth.picking = false;
                else if (Booth.countdown > 0)
                    Booth.cancel();
                else
                    Booth.close();
            } else if (event.key === Qt.Key_Space || event.key === Qt.Key_Return) {
                Booth.shoot();
            } else if (event.key === Qt.Key_E) {
                Booth.picking = !Booth.picking;
            } else if (event.key === Qt.Key_M) {
                Booth.mirror = !Booth.mirror;
            } else if (event.key === Qt.Key_Left || event.key === Qt.Key_Right) {
                Booth.effect = (Booth.effect + (event.key === Qt.Key_Left ? 15 : 1)) % 16;
            } else {
                return;
            }
            event.accepted = true;
        }
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        Rectangle {
            Layout.fillWidth: true
            Layout.preferredHeight: 52
            color: Colors.surface
            topLeftRadius: 16
            topRightRadius: 16

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: 16
                anchors.rightMargin: 12
                spacing: 6

                Text {
                    text: "Photo Booth"
                    color: Colors.textBright
                    font.pixelSize: 13
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }

                Text {
                    Layout.leftMargin: 8
                    text: Booth.effects[Booth.effect]
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                Item { Layout.fillWidth: true }

                Text {
                    visible: Booth.notice.length > 0
                    text: Booth.notice
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                BoothButton { icon: "flip"; active: Booth.mirror; onClicked: Booth.mirror = !Booth.mirror }
                BoothButton { icon: "photo_filter"; active: Booth.picking; onClicked: Booth.picking = !Booth.picking }
                BoothButton { icon: "folder_open"; onClicked: Files.browse(Booth.dir) }
                BoothButton { icon: "close"; onClicked: Booth.close() }
            }
        }

        Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: Colors.border }

        Rectangle {
            id: viewport

            Layout.fillWidth: true
            Layout.fillHeight: true
            color: Colors.backgroundDeep
            bottomLeftRadius: strip.visible ? 0 : 16
            bottomRightRadius: strip.visible ? 0 : 16
            clip: true

            VideoOutput {
                id: vo
                width: root.frameSize.width
                height: root.frameSize.height
                fillMode: VideoOutput.Stretch
            }

            ShaderEffectSource {
                id: feed
                sourceItem: vo
                hideSource: true
                textureSize: root.frameSize
                smooth: true
            }

            ShaderEffect {
                id: shot

                anchors.centerIn: parent
                width: Math.round(root.fitW)
                height: Math.round(root.fitH)
                visible: !Booth.picking

                property var source: feed
                property real aspect: root.aspect
                property real mirror: Booth.mirror ? 1 : 0
                property int effect: Booth.effect
                property size texel: Qt.size(1 / root.frameSize.width, 1 / root.frameSize.height)

                fragmentShader: Qt.resolvedUrl("../../assets/shaders/booth.frag.qsb")
            }

            Grid {
                id: grid

                anchors.centerIn: parent
                visible: Booth.picking
                columns: 4
                spacing: 6

                readonly property real tileW: Math.floor(Math.min((viewport.width - 24 - spacing * 3) / 4, (viewport.height - 24 - spacing * 3) / 4 * root.aspect))
                readonly property real tileH: Math.floor(tileW / root.aspect)

                Repeater {
                    model: Booth.effects

                    Rectangle {
                        id: tile

                        required property string modelData
                        required property int index

                        width: grid.tileW
                        height: grid.tileH
                        radius: 8
                        color: Colors.surface
                        border.width: 2
                        border.color: tile.index === Booth.effect ? Colors.blue : tileHover.hovered ? Colors.outline : "transparent"
                        clip: true

                        ShaderEffect {
                            anchors.fill: parent
                            anchors.margins: 2

                            property var source: feed
                            property real aspect: root.aspect
                            property real mirror: Booth.mirror ? 1 : 0
                            property int effect: tile.index
                            property size texel: shot.texel

                            fragmentShader: Qt.resolvedUrl("../../assets/shaders/booth.frag.qsb")
                        }

                        Rectangle {
                            anchors.left: parent.left
                            anchors.right: parent.right
                            anchors.bottom: parent.bottom
                            anchors.margins: 2
                            height: 22
                            color: Qt.rgba(0, 0, 0, 0.55)

                            Text {
                                anchors.centerIn: parent
                                text: tile.modelData
                                color: Colors.textBright
                                font.pixelSize: 11
                                font.family: Fonts.family
                            }
                        }

                        HoverHandler {
                            id: tileHover
                            cursorShape: Qt.PointingHandCursor
                        }

                        MouseArea {
                            anchors.fill: parent
                            onClicked: Booth.pick(tile.index)
                        }
                    }
                }
            }

            Text {
                anchors.centerIn: parent
                visible: Booth.countdown > 0
                text: Booth.countdown
                color: Colors.textBright
                font.pixelSize: 120
                font.family: Fonts.family
                font.weight: Font.Bold
                style: Text.Outline
                styleColor: Qt.rgba(0, 0, 0, 0.6)
            }

            Rectangle {
                id: shutter

                anchors.horizontalCenter: parent.horizontalCenter
                anchors.bottom: parent.bottom
                anchors.bottomMargin: 20
                width: 56
                height: 56
                radius: 28
                visible: !Booth.picking
                color: Booth.countdown > 0 ? Colors.red : shutterHover.containsMouse ? Colors.textBright : Colors.text
                border.width: 4
                border.color: Qt.rgba(0, 0, 0, 0.45)
                scale: shutterHover.pressed ? 0.92 : 1

                Behavior on color { ColorAnimation { duration: 140 } }
                Behavior on scale { Anim { duration: 120 } }

                MouseArea {
                    id: shutterHover
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: Booth.countdown > 0 ? Booth.cancel() : Booth.shoot()
                }
            }

            Rectangle {
                anchors.fill: parent
                color: "white"
                opacity: Booth.flash ? 1 : 0
                visible: opacity > 0

                Behavior on opacity { Anim { duration: 260 } }
            }
        }

        Rectangle {
            id: strip

            Layout.fillWidth: true
            Layout.preferredHeight: 76
            visible: Booth.shots.length > 0
            color: Colors.surface
            bottomLeftRadius: 16
            bottomRightRadius: 16

            Rectangle { anchors.top: parent.top; width: parent.width; height: 1; color: Colors.border }

            ListView {
                id: shots

                anchors.fill: parent
                anchors.margins: 10
                orientation: ListView.Horizontal
                spacing: 8
                clip: true
                model: Booth.shots

                WheelHandler {
                    onWheel: event => shots.contentX = Math.max(0, Math.min(shots.contentWidth - shots.width, shots.contentX - event.angleDelta.y))
                }

                delegate: Rectangle {
                    id: thumb

                    required property string modelData

                    width: Math.round(height * root.aspect)
                    height: shots.height
                    radius: 6
                    color: Colors.subtle
                    border.width: 1
                    border.color: thumbHover.hovered ? Colors.outline : Colors.border
                    clip: true

                    Image {
                        anchors.fill: parent
                        anchors.margins: 1
                        source: "file://" + thumb.modelData
                        sourceSize.width: 256
                        fillMode: Image.PreserveAspectCrop
                        asynchronous: true
                        cache: false
                    }

                    HoverHandler {
                        id: thumbHover
                        cursorShape: Qt.PointingHandCursor
                    }

                    MouseArea {
                        anchors.fill: parent
                        onClicked: Preview.openFile(thumb.modelData)
                    }
                }
            }
        }
    }

    component BoothButton: Rectangle {
        id: btn

        property string icon: ""
        property bool active: false

        signal clicked

        implicitWidth: 34
        implicitHeight: 34
        radius: 9
        color: btn.active ? Colors.surfaceActive : btnArea.containsMouse ? Colors.subtle : "transparent"

        Behavior on color { ColorAnimation { duration: 120 } }

        MaterialIcon {
            anchors.centerIn: parent
            text: btn.icon
            size: 18
            color: btn.active ? Colors.blue : btnArea.containsMouse ? Colors.textBright : Colors.textDimmed
        }

        MouseArea {
            id: btnArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: btn.clicked()
        }
    }
}
