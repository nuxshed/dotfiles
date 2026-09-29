pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import "../../config"
import "../../services"

Variants {
    model: Quickshell.screens

    PanelWindow {
        id: win

        required property var modelData

        property Image current: null

        screen: modelData
        color: Colors.backgroundDeep
        exclusionMode: ExclusionMode.Ignore

        WlrLayershell.layer: WlrLayer.Background
        WlrLayershell.namespace: "qs:wallpaper"

        anchors {
            top: true
            bottom: true
            left: true
            right: true
        }

        function load(): void {
            if (current && current.path === Wallpapers.shown)
                return;
            current = layer.createObject(stage, { path: Wallpapers.shown });
        }

        Component.onCompleted: load()

        Connections {
            target: Wallpapers

            function onShownChanged(): void {
                win.load();
            }
        }

        Item {
            id: stage
            anchors.fill: parent
        }

        Component {
            id: layer

            Image {
                id: img

                required property string path

                anchors.fill: parent
                source: path.length > 0 ? "file://" + path : ""
                sourceSize: Qt.size(win.width * win.modelData.devicePixelRatio, win.height * win.modelData.devicePixelRatio)
                fillMode: Settings.wallpaperFit === "fit" ? Image.PreserveAspectFit : Image.PreserveAspectCrop
                asynchronous: true
                cache: false
                smooth: true
                opacity: 0
                scale: 1.03

                onStatusChanged: {
                    if (status === Image.Ready && win.current !== img)
                        img.destroy();
                    else if (status === Image.Ready)
                        reveal.start();
                    else if (status === Image.Error)
                        img.destroy();
                }

                ParallelAnimation {
                    id: reveal

                    NumberAnimation {
                        target: img
                        property: "opacity"
                        to: 1
                        duration: 450
                        easing.type: Easing.OutCubic
                    }
                    NumberAnimation {
                        target: img
                        property: "scale"
                        to: 1
                        duration: 700
                        easing.type: Easing.OutCubic
                    }

                    onFinished: {
                        if (win.current !== img)
                            return;
                        for (const child of stage.children)
                            if (child !== img)
                                child.destroy();
                    }
                }
            }
        }
    }
}
