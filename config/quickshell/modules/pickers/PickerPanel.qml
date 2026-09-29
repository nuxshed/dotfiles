pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland
import Quickshell.Widgets
import "../../components"
import "../../config"
import "../../services"

PanelWindow {
    id: root

    readonly property var spatial: [0.38, 1.21, 0.22, 1, 1, 1]
    readonly property var effects: [0.34, 0.8, 0.34, 1, 1, 1]
    readonly property int pad: 12
    readonly property int corner: 24
    readonly property int cardWidth: 256
    readonly property int cardHeight: 144
    readonly property int itemWidth: Math.round(cardWidth * 0.9) + 24
    readonly property int panelHeight: cardHeight + 58 + pad * 2

    readonly property int themeCount: fit(Themes.list.length)
    readonly property int wallCount: fit(Wallpapers.list.length)

    property string mode: Pickers.tab
    property real offset: Pickers.open ? 0 : 1

    readonly property real reveal: Math.max(0, drawer.height - (drawer.height + 5) * offset)

    function fit(total: int): int {
        const n = Math.min(Math.floor((screen.width - 220) / itemWidth), 7, total);
        return n > 1 && n % 2 === 0 ? n - 1 : Math.max(n, 1);
    }

    visible: Pickers.open || offset < 1
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    implicitHeight: panelHeight + 40

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:picker"
    WlrLayershell.keyboardFocus: Pickers.open ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

    anchors {
        bottom: true
        left: true
        right: true
    }

    mask: Region {
        id: region
        item: hit
    }

    onRevealChanged: region.changed()

    Behavior on offset {
        NumberAnimation {
            duration: 500
            easing.type: Easing.BezierSpline
            easing.bezierCurve: root.spatial
        }
    }

    Behavior on mode {
        enabled: Pickers.open

        SequentialAnimation {
            NumberAnimation {
                target: content
                property: "opacity"
                to: 0
                duration: 120
                easing.type: Easing.BezierSpline
                easing.bezierCurve: root.effects
            }
            PropertyAction {}
            NumberAnimation {
                target: content
                property: "opacity"
                to: 1
                duration: 200
                easing.type: Easing.BezierSpline
                easing.bezierCurve: root.effects
            }
        }
    }

    function sync(): void {
        Colors.preview = "";
        Wallpapers.stopPreview();
        themes.currentIndex = Math.max(0, Themes.list.findIndex(t => t.id === Settings.theme));
        walls.currentIndex = Math.max(0, Wallpapers.index);
    }

    function commit(): void {
        if (Pickers.tab === "theme")
            Settings.set("theme", Themes.list[themes.currentIndex].id);
        else if (Wallpapers.list.length > 0)
            Wallpapers.set(Wallpapers.list[walls.currentIndex].path);
        Pickers.close();
    }

    function step(delta: int): void {
        const view = Pickers.tab === "theme" ? themes : walls;
        if (delta > 0)
            view.incrementCurrentIndex();
        else
            view.decrementCurrentIndex();
    }

    Connections {
        target: Pickers

        function onOpenChanged(): void {
            Colors.instant = Pickers.open;
            if (Pickers.open) {
                root.sync();
                keys.forceActiveFocus();
            }
            grab.active = Pickers.open;
        }

        function onTabChanged(): void {
            root.sync();
        }
    }

    HyprlandFocusGrab {
        id: grab
        windows: [root]
        onCleared: if (Pickers.open) Pickers.close()
    }

    Item {
        id: keys

        focus: true

        Keys.onPressed: event => {
            const k = event.key;
            if (k === Qt.Key_Escape)
                Pickers.close();
            else if (k === Qt.Key_Return || k === Qt.Key_Enter)
                root.commit();
            else if (k === Qt.Key_Tab || k === Qt.Key_Backtab)
                Pickers.tab = Pickers.tab === "theme" ? "wallpaper" : "theme";
            else if (k === Qt.Key_Left || k === Qt.Key_Up || k === Qt.Key_H || k === Qt.Key_K)
                root.step(-1);
            else if (k === Qt.Key_Right || k === Qt.Key_Down || k === Qt.Key_L || k === Qt.Key_J)
                root.step(1);
            else
                return;
            event.accepted = true;
        }
    }

    Item {
        id: hit

        x: drawer.x - root.corner
        y: root.height - root.reveal
        width: drawer.width + root.corner * 2
        height: root.reveal
    }

    RoundCorner {
        x: drawer.x - size
        y: root.height - size
        size: Math.min(root.corner, root.reveal)
        color: Colors.background
        corner: RoundCorner.CornerEnum.BottomRight
    }

    RoundCorner {
        x: drawer.x + drawer.width
        y: root.height - size
        size: Math.min(root.corner, root.reveal)
        color: Colors.background
        corner: RoundCorner.CornerEnum.BottomLeft
    }

    Item {
        id: drawer

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: (-height - 5) * root.offset
        width: (root.mode === "wallpaper" ? root.wallCount : root.themeCount) * root.itemWidth + root.pad * 2
        height: root.panelHeight

        Behavior on width {
            enabled: Pickers.open

            NumberAnimation {
                duration: 500
                easing.type: Easing.BezierSpline
                easing.bezierCurve: root.spatial
            }
        }

        Rectangle {
            width: parent.width
            height: parent.height + 40
            radius: root.corner + 4
            bottomLeftRadius: 0
            bottomRightRadius: 0
            color: Colors.background
        }

        Item {
            id: content

            anchors.fill: parent
            clip: true

            Carousel {
                id: themes

                count: root.themeCount
                visible: root.mode === "theme"
                model: Themes.list

                onCurrentIndexChanged: {
                    if (Pickers.open && Pickers.tab === "theme" && Themes.list[currentIndex])
                        Colors.preview = Themes.list[currentIndex].id;
                }

                delegate: Card {
                    id: themeCard

                    required property var modelData

                    view: themes
                    label: modelData.name
                    applied: Settings.theme === modelData.id

                    ThemeSwatch {
                        anchors.fill: parent
                        radius: 16
                        themeId: themeCard.modelData.id
                    }
                }
            }

            Carousel {
                id: walls

                count: root.wallCount
                visible: root.mode === "wallpaper"
                model: Wallpapers.list

                onCurrentIndexChanged: {
                    if (Pickers.open && Pickers.tab === "wallpaper" && Wallpapers.list[currentIndex])
                        Wallpapers.preview(Wallpapers.list[currentIndex].path);
                }

                delegate: Card {
                    id: wallCard

                    required property var modelData

                    view: walls
                    label: modelData.name
                    applied: Settings.wallpaper === modelData.path

                    MaterialIcon {
                        anchors.centerIn: parent
                        text: "image"
                        size: 32
                        color: Colors.outline
                    }

                    Image {
                        anchors.fill: parent
                        source: "file://" + wallCard.modelData.path
                        sourceSize: Qt.size(root.cardWidth * 2, root.cardHeight * 2)
                        fillMode: Image.PreserveAspectCrop
                        asynchronous: true
                        smooth: !walls.moving
                        opacity: status === Image.Ready ? 1 : 0

                        Behavior on opacity {
                            NumberAnimation { duration: 200 }
                        }
                    }
                }
            }

            Text {
                anchors.centerIn: parent
                visible: root.mode === "wallpaper" && Wallpapers.list.length === 0
                text: "No wallpapers in " + Settings.wallpaperDir.replace(Settings.home, "~")
                color: Colors.textMuted
                font.pixelSize: 13
                font.family: Fonts.family
            }
        }
    }

    component Carousel: PathView {
        id: view

        property int count: 1
        property real wheel: 0

        anchors.horizontalCenter: parent.horizontalCenter
        y: root.pad
        width: count * root.itemWidth
        height: parent.height - root.pad * 2
        pathItemCount: count
        cacheItemCount: 4
        snapMode: PathView.SnapToItem
        preferredHighlightBegin: 0.5
        preferredHighlightEnd: 0.5
        highlightRangeMode: PathView.StrictlyEnforceRange
        highlightMoveDuration: 340

        path: Path {
            startX: 0
            startY: view.height / 2

            PathLine { x: view.width; relativeY: 0 }
        }

        WheelHandler {
            onWheel: event => {
                view.wheel += event.angleDelta.y + event.angleDelta.x;
                if (Math.abs(view.wheel) < 120)
                    return;
                if (view.wheel < 0)
                    view.incrementCurrentIndex();
                else
                    view.decrementCurrentIndex();
                view.wheel = 0;
            }
        }
    }

    component Card: Item {
        id: card

        required property int index
        property PathView view: null
        property string label: ""
        property bool applied: false
        default property alias content: frame.data

        readonly property bool current: view !== null && index === view.currentIndex

        width: root.itemWidth
        height: view?.height ?? 0
        z: current ? 1 : 0
        scale: 0.5
        opacity: 0

        Component.onCompleted: {
            scale = Qt.binding(() => card.current ? 1 : 0.8);
            opacity = Qt.binding(() => card.current ? 1 : 0.55);
        }

        Behavior on scale {
            NumberAnimation {
                duration: 450
                easing.type: Easing.BezierSpline
                easing.bezierCurve: root.spatial
            }
        }

        Behavior on opacity {
            NumberAnimation {
                duration: 200
                easing.type: Easing.BezierSpline
                easing.bezierCurve: root.effects
            }
        }

        ClippingRectangle {
            id: frame

            anchors.horizontalCenter: parent.horizontalCenter
            y: 10
            width: root.cardWidth
            height: root.cardHeight
            radius: 16
            color: Colors.surface
        }

        Row {
            anchors.top: frame.bottom
            anchors.topMargin: 10
            anchors.horizontalCenter: parent.horizontalCenter
            spacing: 6

            Rectangle {
                anchors.verticalCenter: parent.verticalCenter
                width: 6
                height: 6
                radius: 3
                color: Colors.primary
                visible: card.applied
            }

            Text {
                width: Math.min(implicitWidth, root.cardWidth - 24)
                text: card.label
                color: card.current ? Colors.textBright : Colors.textMuted
                font.pixelSize: 12
                font.family: Fonts.family
                font.weight: card.current ? Font.Medium : Font.Normal
                elide: Text.ElideRight
            }
        }

        MouseArea {
            anchors.fill: frame
            cursorShape: Qt.PointingHandCursor
            onClicked: {
                card.view.currentIndex = card.index;
                root.commit();
            }
        }
    }
}
