import Quickshell
import Quickshell.Io
import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

FloatingCard {
    id: root

    required property var modelData

    readonly property string path: modelData.path
    readonly property string type: modelData.type
    readonly property string kind: modelData.kind ?? ""
    readonly property var peaks: modelData.peaks ?? []
    readonly property bool isVoice: type === "player" && kind === "voice"
    readonly property bool isVideo: type === "player" && kind === "screen"

    property real duration: 0
    property real position: 0
    property bool playing: false
    property string poster: ""

    signal dismissed

    function clock(seconds) {
        const p = n => String(n).padStart(2, "0")
        const s = Math.max(0, Math.floor(seconds))
        return `${p(Math.floor(s / 60))}:${p(s % 60)}`
    }

    function play() {
        playing = true
        mpv.exec(["mpv", "--no-video", "--really-quiet", `--start=${position.toFixed(2)}`, path])
    }

    function pause() {
        playing = false
        mpv.running = false
    }

    function seek(fraction) {
        position = Math.max(0, Math.min(duration, fraction * duration))
        if (playing)
            play()
    }

    posX: 120 + (modelData.uid % 6) * 26
    posY: 120 + (modelData.uid % 6) * 26

    cardWidth: isVoice ? 340 : 380
    cardHeight: {
        if (isVoice)
            return 108
        if (isVideo)
            return 260
        return shot.implicitHeight > 0 ? Math.min(420, 380 * shot.implicitHeight / Math.max(1, shot.implicitWidth)) + 44 : 280
    }

    Component.onCompleted: {
        if (type !== "player")
            return
        probe.exec(["bash", "-c", `ffprobe -v error -show_entries format=duration -of csv=p=0 '${path}'`])
        if (isVideo) {
            poster = `/tmp/qs-capture/poster-${modelData.uid}.png`
            posterProc.exec(["bash", "-c", `ffmpeg -y -loglevel error -ss 0.5 -i '${path}' -frames:v 1 -vf scale=760:-1 '${poster}'`])
        }
    }

    Process {
        id: probe

        stdout: StdioCollector {
            onStreamFinished: root.duration = parseFloat(text.trim()) || 0
        }
    }

    Process {
        id: posterProc
        onExited: code => {
            if (code === 0)
                posterImage.source = "file://" + root.poster
        }
    }

    Process {
        id: mpv
        onExited: {
            if (!root.playing)
                return
            root.playing = false
            root.position = 0
        }
    }

    Timer {
        running: root.playing
        interval: 100
        repeat: true
        onTriggered: {
            root.position += 0.1
            if (root.duration > 0 && root.position >= root.duration) {
                root.pause()
                root.position = 0
            }
        }
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 10
        spacing: 8

        Image {
            id: shot
            visible: root.type === "pin"
            Layout.fillWidth: true
            Layout.fillHeight: true
            source: root.type === "pin" ? "file://" + root.path : ""
            fillMode: Image.PreserveAspectFit
            cache: false
        }

        Image {
            id: posterImage
            visible: root.isVideo
            Layout.fillWidth: true
            Layout.fillHeight: true
            fillMode: Image.PreserveAspectFit
            cache: false
        }

        RowLayout {
            visible: root.isVoice
            Layout.fillWidth: true
            Layout.fillHeight: true
            spacing: 10

            CardButton {
                icon: root.playing ? "pause" : "play_arrow"
                onActivated: root.playing ? root.pause() : root.play()
            }

            Canvas {
                id: wave

                Layout.fillWidth: true
                Layout.preferredHeight: 34

                readonly property var bars: Recorder.resample(root.peaks, 44)
                readonly property real progress: root.duration > 0 ? root.position / root.duration : 0

                onProgressChanged: requestPaint()
                onBarsChanged: requestPaint()

                onPaint: {
                    const ctx = getContext("2d")
                    ctx.clearRect(0, 0, width, height)

                    const gap = 3
                    const barWidth = Math.max(1, (width - gap * (bars.length - 1)) / bars.length)
                    const mid = height / 2

                    for (let i = 0; i < bars.length; i++) {
                        const h = Math.max(2, bars[i] * height)
                        const x = i * (barWidth + gap)
                        ctx.fillStyle = (i / bars.length) <= progress ? Colors.textBright : Colors.subtle
                        ctx.beginPath()
                        ctx.roundedRect(x, mid - h / 2, barWidth, h, barWidth / 2, barWidth / 2)
                        ctx.fill()
                    }
                }

                MouseArea {
                    anchors.fill: parent
                    cursorShape: Qt.PointingHandCursor
                    onClicked: mouse => root.seek(mouse.x / width)
                }
            }

            Text {
                Layout.preferredWidth: 40
                text: root.clock(root.playing || root.position > 0 ? root.position : root.duration)
                color: Colors.textMuted
                font.pixelSize: 12
                horizontalAlignment: Text.AlignRight
            }
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: 6

            Text {
                Layout.fillWidth: true
                text: root.path.split("/").pop()
                color: Colors.textMuted
                font.pixelSize: 11
                elide: Text.ElideMiddle
            }

            CardButton {
                icon: "open_in_new"
                visible: root.isVideo
                onActivated: Quickshell.execDetached(["xdg-open", root.path])
            }

            CardButton {
                icon: "content_copy"
                visible: root.type === "pin"
                onActivated: Quickshell.execDetached(["bash", "-c", `wl-copy -t image/png < '${root.path}'`])
            }

            CardButton {
                icon: "save"
                visible: root.type === "pin"
                onActivated: Quickshell.execDetached(["bash", "-c", `mkdir -p '${Capture.shotDir}' && cp '${root.path}' '${Capture.shotDir}/Screenshot_${Capture.stamp()}.png'`])
            }

            CardButton {
                icon: "close"
                onActivated: {
                    root.pause()
                    if (root.type === "pin")
                        Quickshell.execDetached(["bash", "-c", `rm -f '${root.path}'`])
                    if (root.poster)
                        Quickshell.execDetached(["bash", "-c", `rm -f '${root.poster}'`])
                    root.dismissed()
                }
            }
        }
    }

    component CardButton: Rectangle {
        id: btn

        property string icon: ""

        signal activated

        Layout.preferredWidth: 28
        Layout.preferredHeight: 28
        radius: 6
        color: area.containsMouse ? Colors.surfaceActive : Colors.surface

        Behavior on color {
            ColorAnimation {
                duration: 140
            }
        }

        MaterialIcon {
            anchors.centerIn: parent
            text: btn.icon
            size: 15
            color: Colors.textDimmed
        }

        MouseArea {
            id: area
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: btn.activated()
        }
    }
}
