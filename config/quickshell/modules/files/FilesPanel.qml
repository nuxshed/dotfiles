pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import "../../components"
import "../../config"
import "../../services"

PanelWindow {
    id: root

    property int cursor: -1
    property bool editingPath: false
    property bool dragging: false
    property real savedScroll: 0
    property bool restoreScroll: false

    visible: Files.open
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:files"
    WlrLayershell.keyboardFocus: Files.open ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    mask: root.dragging ? cardMask : null

    Region {
        id: cardMask
        item: card
    }

    function focusList(): void {
        root.editingPath = false;
        menu.shown = false;
        keys.forceActiveFocus();
    }

    readonly property int columns: Files.view === "grid" ? Math.max(1, Math.floor(grid.width / grid.cellWidth)) : 1

    function moveCursor(delta: int, extend: bool): void {
        const n = Files.items.length;
        if (n === 0)
            return;
        root.cursor = Math.max(0, Math.min(n - 1, root.cursor + delta));
        Files.select(Files.items[root.cursor], false, extend);
        if (Files.view === "grid")
            grid.positionViewAtIndex(root.cursor, GridView.Contain);
        else
            list.positionViewAtIndex(root.cursor, ListView.Contain);
    }

    function startSearch(): void {
        if (!Files.searching) {
            search.text = "";
            Files.startSearch();
        }
        search.input.forceActiveFocus();
    }

    function activateCursor(): void {
        if (root.cursor >= 0 && root.cursor < Files.items.length)
            Files.activate(Files.items[root.cursor]);
        else
            Files.accept();
    }

    function entryMenu(): var {
        const n = Files.selected.length;
        const one = n === 1 ? Files.selectedEntries[0] : null;
        const items = [];
        if (one)
            items.push({ label: one.isDir ? "Open" : "Open with default app", icon: one.isDir ? "folder_open" : "open_in_new", act: () => Files.activate(one) });
        if (one && one.isDir)
            items.push({ label: "Open in terminal", icon: "computer", act: () => Files.openTerminal(one.path) });
        if (one && !one.isDir)
            items.push({ label: "Open with…", icon: "apps", shortcut: "Ctrl+O", act: () => Files.askOpenWith() });
        if (one && one.isDir)
            items.push({ label: "Open in new tab", icon: "tab", act: () => Files.newTab(one.path) });
        if (one && (Files.searching || Files.special.length > 0))
            items.push({ label: "Reveal in folder", icon: "folder_open", shortcut: "Alt+Enter", act: () => Files.reveal(one) });
        if (one && one.isDir)
            items.push({ label: Files.isPinned(one.path) ? "Unpin from sidebar" : "Pin to sidebar", icon: "bookmark", act: () => Files.isPinned(one.path) ? Files.unpin(one.path) : Files.pin(one.path) });
        items.push({ label: "Cut", icon: "content_cut", shortcut: "Ctrl+X", divider: true, act: () => Files.cut() });
        items.push({ label: "Copy", icon: "content_copy", shortcut: "Ctrl+C", act: () => Files.copy() });
        items.push({ label: "Copy path", icon: "link", shortcut: "Ctrl+Shift+C", act: () => Files.copyPath() });
        items.push({ label: "Rename", icon: "edit", shortcut: "F2", divider: true, enabled: n === 1, act: () => Files.askRename() });
        items.push({ label: "Move to trash", icon: "delete", shortcut: "Del", danger: true, act: () => Files.askDelete(false) });
        items.push({ label: "Delete permanently", icon: "delete_forever", shortcut: "Shift+Del", danger: true, act: () => Files.askDelete(true) });
        items.push({ label: "Properties", icon: "info", shortcut: "Alt+Enter", divider: true, enabled: n === 1 && !Files.searching, act: () => Files.askProperties() });
        return items;
    }

    function folderMenu(): var {
        return [
            { label: "New folder", icon: "create_new_folder", shortcut: "Ctrl+Shift+N", act: () => Files.askNewFolder() },
            { label: "Paste", icon: "content_paste", shortcut: "Ctrl+V", enabled: Files.clip.paths.length > 0, act: () => Files.paste() },
            { label: "New tab", icon: "tab", shortcut: "Ctrl+T", divider: true, act: () => Files.newTab("") },
            { label: "Open in terminal", icon: "computer", shortcut: "Ctrl+Shift+T", act: () => Files.openTerminal() },
            { label: Files.isPinned(Files.cwd) ? "Unpin from sidebar" : "Pin to sidebar", icon: "bookmark", act: () => Files.isPinned(Files.cwd) ? Files.unpin(Files.cwd) : Files.pin(Files.cwd) },
            { label: "Copy path", icon: "link", act: () => { Files.selected = []; Files.copyPath(); } },
            { label: "Refresh", icon: "refresh", shortcut: "F5", act: () => Files.refresh() },
            { label: "Sort by name", icon: Files.sortBy === "name" ? "check" : "", divider: true, act: () => Files.setSort("name") },
            { label: "Sort by modified", icon: Files.sortBy === "modified" ? "check" : "", act: () => Files.setSort("modified") },
            { label: "Sort by size", icon: Files.sortBy === "size" ? "check" : "", act: () => Files.setSort("size") },
            { label: "Properties", icon: "info", divider: true, act: () => { Files.selected = []; Files.askProperties(); } }
        ];
    }

    function beginDrag(entry: var, modifiers: int): void {
        if (!Files.selected.includes(entry.path))
            Files.select(entry, false, false);
        Files.dragPaths = Files.arr(Files.selected);
        ghost.copy = (modifiers & Qt.ControlModifier) !== 0;
        root.dragging = true;
        ghost.Drag.active = true;
        ghost.grabToImage(result => {
            if (root.dragging)
                ghost.Drag.imageSource = result.url;
        });
    }

    function endDrag(): void {
    }

    function finishDrag(): void {
        root.dragging = false;
        Files.dragPaths = [];
    }

    function rowClicked(index: int, entry: var, modifiers: int): void {
        root.cursor = index;
        root.focusList();
        Files.select(entry, modifiers & Qt.ControlModifier, modifiers & Qt.ShiftModifier);
    }

    function rowActivated(index: int, entry: var): void {
        root.cursor = index;
        Files.activate(entry);
    }

    function rowMenu(index: int, entry: var, p: point): void {
        if (Files.picker)
            return;
        root.cursor = index;
        root.focusList();
        if (!Files.selected.includes(entry.path))
            Files.select(entry, false, false);
        root.showMenu(root.entryMenu(), p.x, p.y);
    }

    function showMenu(items: var, x: real, y: real): void {
        menu.items = items;
        menu.popup(x, y, card.width, card.height);
    }

    onVisibleChanged: {
        if (visible) {
            root.cursor = -1;
            root.editingPath = false;
            menu.shown = false;
            if (Files.save)
                nameField.input.forceActiveFocus();
            else
                keys.forceActiveFocus();
        }
    }

    Connections {
        target: Files

        function onSearchingChanged() {
            if (!Files.searching)
                search.text = "";
        }

        function onRefreshing(clear) {
            root.restoreScroll = !clear;
            root.savedScroll = Files.view === "grid" ? grid.contentY : list.contentY;
        }

        function onCwdChanged() {
            root.cursor = -1;
            root.editingPath = false;
            menu.shown = false;
            list.positionViewAtBeginning();
            grid.positionViewAtBeginning();
        }

        function onGenerationChanged() {
            search.text = "";
            root.dragging = false;
            nameField.text = Files.fileName;
        }

        function onItemsChanged() {
            if (root.cursor >= Files.items.length)
                root.cursor = -1;
            if (root.restoreScroll && Files.items.length > 0) {
                const y = root.savedScroll;
                root.restoreScroll = false;
                Qt.callLater(() => {
                    const v = Files.view === "grid" ? grid : list;
                    v.contentY = Math.max(-v.topMargin, Math.min(Math.max(-v.topMargin, v.contentHeight - v.height + v.bottomMargin), y));
                });
            }
        }
    }

    Rectangle {
        anchors.fill: parent
        color: "#000000"
        opacity: !Files.open ? 0 : root.dragging ? 0.15 : 0.45

        Behavior on opacity {
            NumberAnimation { duration: 150 }
        }

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.AllButtons
            onClicked: Files.hide()
        }
    }

    Rectangle {
        id: card

        anchors.centerIn: parent
        width: Math.min(980, parent.width - 80)
        height: Math.min(640, parent.height - 80)
        radius: 16
        color: Colors.background
        border.color: Colors.border
        border.width: 1
        clip: true
        opacity: Files.open ? 1 : 0
        scale: Files.open ? 1 : 0.97

        Behavior on opacity {
            Anim { duration: 160 }
        }

        Behavior on scale {
            Anim { duration: 160 }
        }

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.LeftButton | Qt.RightButton
            onClicked: mouse => {
                root.focusList();
                if (mouse.button === Qt.RightButton && !Files.picker)
                    root.showMenu(root.folderMenu(), mouse.x, mouse.y);
            }
        }

        Item {
            id: keys

            anchors.fill: parent
            focus: true

            Keys.onPressed: event => {
                if (Files.dialog || Files.openWith || Files.props)
                    return;

                const ctrl = event.modifiers & Qt.ControlModifier;
                const alt = event.modifiers & Qt.AltModifier;
                const shift = event.modifiers & Qt.ShiftModifier;
                const browse = !Files.picker;

                if (menu.shown && event.key !== Qt.Key_Escape)
                    menu.shown = false;

                if (event.key === Qt.Key_Escape) {
                    if (menu.shown)
                        menu.shown = false;
                    else if (Files.selected.length > 0 && browse)
                        Files.selected = [];
                    else
                        Files.hide();
                } else if (event.key === Qt.Key_Down) {
                    root.moveCursor(root.columns, shift);
                } else if (event.key === Qt.Key_Up) {
                    root.moveCursor(-root.columns, shift);
                } else if (event.key === Qt.Key_Right && Files.view === "grid") {
                    root.moveCursor(1, shift);
                } else if (event.key === Qt.Key_Left && Files.view === "grid") {
                    root.moveCursor(-1, shift);
                } else if (event.key === Qt.Key_Home) {
                    root.moveCursor(-Files.items.length, shift);
                } else if (event.key === Qt.Key_End) {
                    root.moveCursor(Files.items.length, shift);
                } else if (event.key === Qt.Key_PageDown && !ctrl) {
                    root.moveCursor(10, shift);
                } else if (event.key === Qt.Key_PageUp && !ctrl) {
                    root.moveCursor(-10, shift);
                } else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                    if (ctrl)
                        Files.accept();
                    else if (alt && root.cursor >= 0 && root.cursor < Files.items.length && (Files.searching || Files.special.length > 0))
                        Files.reveal(Files.items[root.cursor]);
                    else if (alt && browse)
                        Files.askProperties();
                    else
                        root.activateCursor();
                } else if (event.key === Qt.Key_Backspace || (alt && event.key === Qt.Key_Up)) {
                    Files.up();
                } else if (alt && event.key === Qt.Key_Left) {
                    Files.back();
                } else if (alt && event.key === Qt.Key_Right) {
                    Files.forward();
                } else if (ctrl && (event.key === Qt.Key_1 || event.key === Qt.Key_2)) {
                    Files.setView(event.key === Qt.Key_1 ? "list" : "grid");
                } else if (event.key === Qt.Key_F5 || (ctrl && event.key === Qt.Key_R)) {
                    Files.refresh();
                } else if (ctrl && event.key === Qt.Key_H) {
                    Files.showHidden = !Files.showHidden;
                } else if (ctrl && event.key === Qt.Key_L) {
                    root.editingPath = true;
                } else if ((ctrl && (event.key === Qt.Key_F || event.key === Qt.Key_K)) || (event.key === Qt.Key_Slash && !ctrl && !alt)) {
                    root.startSearch();
                } else if (ctrl && event.key === Qt.Key_A) {
                    Files.selectAll();
                } else if (ctrl && shift && event.key === Qt.Key_C) {
                    Files.copyPath();
                } else if (ctrl && shift && event.key === Qt.Key_N && (browse || Files.save)) {
                    Files.askNewFolder();
                } else if (ctrl && shift && event.key === Qt.Key_T && browse) {
                    Files.openTerminal();
                } else if (ctrl && event.key === Qt.Key_T && browse) {
                    Files.newTab("");
                } else if (ctrl && event.key === Qt.Key_W && browse) {
                    Files.closeTab(Files.tab);
                } else if (ctrl && (event.key === Qt.Key_Tab || event.key === Qt.Key_Backtab) && browse) {
                    Files.cycleTab(event.key === Qt.Key_Backtab || shift ? -1 : 1);
                } else if (ctrl && event.key === Qt.Key_PageDown && browse) {
                    Files.cycleTab(1);
                } else if (ctrl && event.key === Qt.Key_PageUp && browse) {
                    Files.cycleTab(-1);
                } else if (alt && event.key === Qt.Key_Return && browse && root.cursor >= 0 && !Files.searching) {
                    Files.askProperties();
                } else if (ctrl && event.key === Qt.Key_O && browse) {
                    Files.askOpenWith();
                } else if (ctrl && event.key === Qt.Key_X && browse) {
                    Files.cut();
                } else if (ctrl && event.key === Qt.Key_C && browse) {
                    Files.copy();
                } else if (ctrl && event.key === Qt.Key_V && browse) {
                    Files.paste();
                } else if (event.key === Qt.Key_F2 && browse) {
                    Files.askRename();
                } else if (event.key === Qt.Key_Delete && browse) {
                    Files.askDelete(shift);
                } else if (event.key === Qt.Key_Menu && browse) {
                    root.showMenu(Files.selected.length > 0 ? root.entryMenu() : root.folderMenu(), card.width / 2, card.height / 2);
                } else if (event.text.length > 0 && !ctrl && !alt && event.text.trim().length > 0) {
                    search.text = event.text;
                    search.input.forceActiveFocus();
                    search.input.cursorPosition = search.text.length;
                } else {
                    return;
                }
                event.accepted = true;
            }
        }

        MouseArea {
            id: cardDrag

            property real sx: 0
            property real sy: 0
            property real ox: 0
            property real oy: 0

            width: parent.width
            height: 58 + (tabRow.visible ? 38 : 0)
            onClicked: root.focusList()
            onPressed: mouse => {
                const p = mapToItem(null, mouse.x, mouse.y);
                sx = p.x;
                sy = p.y;
                ox = card.anchors.horizontalCenterOffset;
                oy = card.anchors.verticalCenterOffset;
            }
            onPositionChanged: mouse => {
                const p = mapToItem(null, mouse.x, mouse.y);
                card.anchors.horizontalCenterOffset = ox + p.x - sx;
                card.anchors.verticalCenterOffset = oy + p.y - sy;
            }
        }

        Resizer {
            centered: true
            minWidth: 560
            minHeight: 400
        }

        ColumnLayout {
            anchors.fill: parent
            spacing: 0

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: false
                Layout.preferredHeight: 58
                Layout.leftMargin: 14
                Layout.rightMargin: 14
                spacing: 4

                IconButton {
                    icon: "arrow_back"
                    enabled: Files.backStack.length > 0
                    onClicked: Files.back()
                }

                IconButton {
                    icon: "arrow_forward"
                    enabled: Files.forwardStack.length > 0
                    onClicked: Files.forward()
                }

                IconButton {
                    icon: "arrow_upward"
                    enabled: Files.cwd !== "/"
                    onClicked: Files.up()
                }

                Rectangle {
                    Layout.fillWidth: true
                    Layout.leftMargin: 6
                    Layout.preferredHeight: 34
                    radius: 10
                    color: Colors.surface
                    border.width: 1
                    border.color: pathInput.activeFocus ? Colors.primary : Colors.border
                    clip: true

                    Behavior on border.color {
                        ColorAnimation { duration: 150 }
                    }

                    Flickable {
                        anchors.fill: parent
                        anchors.leftMargin: 6
                        anchors.rightMargin: 6
                        visible: !root.editingPath
                        contentWidth: crumbRow.width
                        contentX: Math.max(0, crumbRow.width - width)
                        interactive: false
                        clip: true

                        MouseArea {
                            anchors.fill: parent
                            onClicked: root.editingPath = true
                        }

                        Row {
                            id: crumbRow

                            height: parent.height
                            spacing: 0

                            Repeater {
                                model: Files.crumbs

                                Row {
                                    id: crumb

                                    required property var modelData
                                    required property int index

                                    readonly property bool last: index === Files.crumbs.length - 1

                                    height: parent.height
                                    spacing: 0

                                    MaterialIcon {
                                        visible: crumb.index > 0
                                        height: parent.height
                                        text: "chevron_right"
                                        size: 16
                                        color: Colors.textMuted
                                    }

                                    Rectangle {
                                        readonly property bool dropTarget: crumbDrop.containsDrag && !crumb.last

                                        height: 24
                                        anchors.verticalCenter: parent.verticalCenter
                                        width: crumbLabel.width + 12
                                        radius: 6
                                        color: dropTarget ? Colors.subtle : crumbMouse.containsMouse ? Colors.surfaceActive : "transparent"
                                        border.width: dropTarget ? 1 : 0
                                        border.color: Colors.primary

                                        DropArea {
                                            id: crumbDrop
                                            anchors.fill: parent
                                            onDropped: event => {
                                                if (!crumb.last)
                                                    Files.dropOn(crumb.modelData.path, Files.dragPaths, event.proposedAction === Qt.CopyAction);
                                            }
                                        }

                                        Text {
                                            id: crumbLabel
                                            anchors.centerIn: parent
                                            text: crumb.modelData.name
                                            color: crumb.last ? Colors.textBright : Colors.textDimmed
                                            font.pixelSize: 12
                                            font.family: Fonts.family
                                            font.weight: crumb.last ? Font.Medium : Font.Normal
                                        }

                                        MouseArea {
                                            id: crumbMouse
                                            anchors.fill: parent
                                            hoverEnabled: true
                                            cursorShape: Qt.PointingHandCursor
                                            onClicked: Files.navigate(crumb.modelData.path)
                                        }
                                    }
                                }
                            }
                        }
                    }

                    TextInput {
                        id: pathInput

                        anchors.fill: parent
                        anchors.leftMargin: 12
                        anchors.rightMargin: 12
                        visible: root.editingPath
                        color: Colors.textBright
                        font.pixelSize: 12
                        font.family: Fonts.family
                        selectByMouse: true
                        selectionColor: Colors.primaryContainer
                        selectedTextColor: Colors.textBright
                        clip: true
                        verticalAlignment: TextInput.AlignVCenter

                        onVisibleChanged: {
                            if (visible) {
                                text = Files.cwd;
                                selectAll();
                                forceActiveFocus();
                            }
                        }

                        onAccepted: {
                            Files.navigate(text);
                            root.focusList();
                        }

                        Keys.onEscapePressed: root.focusList()
                    }
                }

                Field {
                    id: search

                    Layout.preferredWidth: Files.searching ? 260 : 190
                    Layout.leftMargin: 6
                    icon: "search"
                    placeholder: Files.searching ? "Search everywhere…" : "Filter"
                    border.color: Files.searching ? Colors.primary : input.activeFocus ? Colors.primary : Colors.border

                    Behavior on Layout.preferredWidth {
                        Anim { duration: 160 }
                    }

                    onTextChanged: {
                        if (Files.searching)
                            Files.setSearchQuery(text);
                        else
                            Files.query = text;
                        root.cursor = -1;
                    }

                    onAccepted: {
                        if (Files.items.length > 0) {
                            root.cursor = 0;
                            root.focusList();
                            root.activateCursor();
                        }
                    }

                    onEscaped: {
                        if (Files.searching) {
                            text = "";
                            Files.stopSearch();
                            root.focusList();
                        } else if (text.length > 0) {
                            text = "";
                        } else {
                            root.focusList();
                        }
                    }

                    onDown: {
                        root.focusList();
                        root.moveCursor(1, false);
                    }
                }

                IconButton {
                    Layout.leftMargin: 2
                    visible: !Files.picker || Files.save
                    icon: "create_new_folder"
                    onClicked: Files.askNewFolder()
                }

                IconButton {
                    Layout.leftMargin: Files.picker ? 2 : 0
                    icon: Files.view === "grid" ? "view_list" : "view_module"
                    onClicked: Files.setView(Files.view === "grid" ? "list" : "grid")
                }

                IconButton {
                    icon: "refresh"
                    onClicked: Files.refresh()
                }

                IconButton {
                    icon: Files.showHidden ? "visibility" : "visibility_off"
                    active: Files.showHidden
                    onClicked: Files.showHidden = !Files.showHidden
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 1
                color: Colors.border
            }

            RowLayout {
                id: tabRow

                visible: !Files.picker && Files.tabs.length > 1
                Layout.fillWidth: true
                Layout.fillHeight: false
                Layout.preferredHeight: 38
                Layout.leftMargin: 12
                Layout.rightMargin: 12
                spacing: 4

                Repeater {
                    model: Files.tabs

                    Rectangle {
                        id: tabChip

                        required property var modelData
                        required property int index

                        readonly property bool active: index === Files.tab

                        Layout.preferredWidth: Math.min(200, tabLabel.implicitWidth + 44)
                        Layout.preferredHeight: 28
                        radius: 8
                        color: active ? Colors.surfaceActive : tabMouse.containsMouse ? Colors.surface : "transparent"

                        Text {
                            id: tabLabel
                            anchors.left: parent.left
                            anchors.right: tabClose.left
                            anchors.leftMargin: 12
                            anchors.rightMargin: 4
                            anchors.verticalCenter: parent.verticalCenter
                            text: Files.basename(tabChip.modelData.cwd) || "/"
                            color: tabChip.active ? Colors.textBright : Colors.textDimmed
                            font.pixelSize: 11
                            font.family: Fonts.family
                            elide: Text.ElideRight
                        }

                        MouseArea {
                            id: tabMouse
                            anchors.fill: parent
                            hoverEnabled: true
                            acceptedButtons: Qt.LeftButton | Qt.MiddleButton
                            onClicked: mouse => {
                                if (mouse.button === Qt.MiddleButton)
                                    Files.closeTab(tabChip.index);
                                else
                                    Files.switchTab(tabChip.index, false);
                            }
                        }

                        Rectangle {
                            id: tabClose
                            anchors.right: parent.right
                            anchors.rightMargin: 6
                            anchors.verticalCenter: parent.verticalCenter
                            width: 18
                            height: 18
                            radius: 9
                            color: closeMouse.containsMouse ? Colors.subtle : "transparent"
                            opacity: tabChip.active || tabMouse.containsMouse ? 1 : 0

                            MaterialIcon {
                                anchors.centerIn: parent
                                text: "close"
                                size: 13
                                color: Colors.textMuted
                            }

                            MouseArea {
                                id: closeMouse
                                anchors.fill: parent
                                hoverEnabled: true
                                onClicked: Files.closeTab(tabChip.index)
                            }
                        }
                    }
                }

                IconButton {
                    implicitWidth: 28
                    implicitHeight: 28
                    icon: "add"
                    onClicked: Files.newTab("")
                }

                Item { Layout.fillWidth: true }
            }

            Rectangle {
                visible: !Files.picker && Files.tabs.length > 1
                Layout.fillWidth: true
                Layout.preferredHeight: 1
                color: Colors.border
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: 0

                ColumnLayout {
                    id: sidebar

                    Layout.fillWidth: false
                    Layout.preferredWidth: implicitWidth
                    Layout.fillHeight: true
                    Layout.topMargin: 10
                    Layout.leftMargin: 8
                    Layout.rightMargin: 8
                    spacing: 2

                    Text {
                        Layout.leftMargin: 10
                        Layout.bottomMargin: 4
                        text: "Places"
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                        font.capitalization: Font.AllUppercase
                        font.letterSpacing: 0.6
                    }

                    Place {
                        visible: !Files.picker || (!Files.save && !Files.directory)
                        name: "Recent"
                        icon: "history"
                        path: ""
                        special: "recent"
                        onActivated: Files.showRecent()
                    }

                    Repeater {
                        model: Files.places

                        Place {
                            required property var modelData

                            name: modelData.name
                            icon: modelData.icon
                            path: modelData.path
                        }
                    }

                    Text {
                        visible: Files.pinned.length > 0
                        Layout.leftMargin: 10
                        Layout.topMargin: 12
                        Layout.bottomMargin: 4
                        text: "Pinned"
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                        font.capitalization: Font.AllUppercase
                        font.letterSpacing: 0.6
                    }

                    Repeater {
                        model: Files.pinned

                        Place {
                            id: pin

                            required property var modelData

                            name: modelData.name
                            icon: "bookmark"
                            path: modelData.path

                            onRightClicked: (x, y) => {
                                const p = mapToItem(card, x, y);
                                root.showMenu([{ label: "Unpin", icon: "close", act: () => Files.unpin(pin.path) }], p.x, p.y);
                            }
                        }
                    }

                    Item { Layout.fillHeight: true }
                }

                Rectangle {
                    Layout.fillHeight: true
                    Layout.preferredWidth: 1
                    color: Colors.border
                }

                ColumnLayout {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    spacing: 0

                    RowLayout {
                        visible: Files.view === "list"
                        Layout.fillWidth: true
                        Layout.fillHeight: false
                        Layout.preferredHeight: 30
                        Layout.leftMargin: 8 + 10 + 18 + 10
                        Layout.rightMargin: 8 + 12
                        spacing: 10

                        Repeater {
                            model: Files.searching ? [
                                { label: "Name", field: "", width: 0, right: false },
                                { label: "Location", field: "", width: 220, right: true }
                            ] : [
                                { label: "Name", field: "name", width: 0, right: false },
                                { label: "Modified", field: "modified", width: 120, right: false },
                                { label: "Size", field: "size", width: 64, right: true }
                            ]

                            Item {
                                id: header

                                required property var modelData

                                readonly property bool active: modelData.field.length > 0 && Files.sortBy === modelData.field

                                Layout.fillWidth: modelData.width === 0
                                Layout.preferredWidth: modelData.width
                                Layout.fillHeight: true

                                Row {
                                    anchors.verticalCenter: parent.verticalCenter
                                    anchors.right: header.modelData.right ? parent.right : undefined
                                    spacing: 2
                                    layoutDirection: header.modelData.right ? Qt.RightToLeft : Qt.LeftToRight

                                    Text {
                                        text: header.modelData.label
                                        color: header.active || headerMouse.containsMouse ? Colors.textDimmed : Colors.textMuted
                                        font.pixelSize: 10
                                        font.family: Fonts.family
                                        font.capitalization: Font.AllUppercase
                                        font.letterSpacing: 0.6
                                    }

                                    MaterialIcon {
                                        visible: header.active
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: Files.sortDesc ? "arrow_drop_down" : "arrow_drop_up"
                                        size: 16
                                        color: Colors.textDimmed
                                    }
                                }

                                MouseArea {
                                    id: headerMouse
                                    anchors.fill: parent
                                    enabled: header.modelData.field.length > 0
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: Files.setSort(header.modelData.field)
                                }
                            }
                        }
                    }

                    Item {
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        Layout.leftMargin: 8
                        Layout.rightMargin: 8
                        Layout.bottomMargin: 8

                        GridView {
                            id: grid

                            anchors.fill: parent
                            visible: Files.view === "grid"
                            clip: true
                            model: Files.view === "grid" ? Files.items : []
                            topMargin: 10
                            bottomMargin: 6
                            cellWidth: Math.floor(width / Math.max(1, Math.floor(width / 136)))
                            cellHeight: 148
                            boundsBehavior: Flickable.StopAtBounds
                            interactive: false
                            reuseItems: true

                            WheelHandler {
                                acceptedDevices: PointerDevice.Mouse | PointerDevice.TouchPad
                                onWheel: event => {
                                    const step = event.pixelDelta.y !== 0 ? event.pixelDelta.y * 3 : event.angleDelta.y / 120 * 180;
                                    const lo = -grid.topMargin;
                                    grid.contentY = Math.max(lo, Math.min(Math.max(lo, grid.contentHeight - grid.height + grid.bottomMargin), grid.contentY - step));
                                }
                            }

                            delegate: FileTile {
                                required property int index
                                required property var modelData

                                width: grid.cellWidth - 4
                                height: grid.cellHeight - 4
                                entry: modelData
                                selected: Files.selected.includes(modelData.path)
                                current: index === root.cursor
                                dimmed: Files.directory && !modelData.isDir
                                cut: Files.clip.op === "cut" && Files.clip.paths.includes(modelData.path)
                                dragGhost: Files.picker ? null : ghost

                                onDragStarted: modifiers => root.beginDrag(modelData, modifiers)
                                onDragEnded: root.endDrag()
                                onClicked: modifiers => root.rowClicked(index, modelData, modifiers)
                                onDoubleClicked: root.rowActivated(index, modelData)
                                onRightClicked: (x, y) => root.rowMenu(index, modelData, mapToItem(card, x, y))
                                onMiddleClicked: if (modelData.isDir && !Files.picker) Files.newTab(modelData.path)
                            }
                        }

                        ListView {
                            id: list

                            anchors.fill: parent
                            visible: Files.view === "list"
                            clip: true
                            model: Files.view === "list" ? Files.items : []
                            spacing: 1
                            boundsBehavior: Flickable.StopAtBounds
                            interactive: false
                            reuseItems: true

                            WheelHandler {
                                acceptedDevices: PointerDevice.Mouse | PointerDevice.TouchPad
                                onWheel: event => {
                                    const step = event.pixelDelta.y !== 0 ? event.pixelDelta.y * 3 : event.angleDelta.y / 120 * 180;
                                    const lo = -list.topMargin;
                                    list.contentY = Math.max(lo, Math.min(Math.max(lo, list.contentHeight - list.height + list.bottomMargin), list.contentY - step));
                                }
                            }

                            delegate: FileRow {
                                required property int index
                                required property var modelData

                                width: list.width
                                entry: modelData
                                selected: Files.selected.includes(modelData.path)
                                current: index === root.cursor
                                dimmed: Files.directory && !modelData.isDir
                                cut: Files.clip.op === "cut" && Files.clip.paths.includes(modelData.path)
                                dragGhost: Files.picker ? null : ghost

                                onDragStarted: modifiers => root.beginDrag(modelData, modifiers)
                                onDragEnded: root.endDrag()
                                onClicked: modifiers => root.rowClicked(index, modelData, modifiers)
                                onDoubleClicked: root.rowActivated(index, modelData)
                                onRightClicked: (x, y) => root.rowMenu(index, modelData, mapToItem(card, x, y))
                                onMiddleClicked: if (modelData.isDir && !Files.picker) Files.newTab(modelData.path)
                            }
                        }

                        Column {
                            anchors.centerIn: parent
                            visible: Files.items.length === 0
                            spacing: 6

                            MaterialIcon {
                                anchors.horizontalCenter: parent.horizontalCenter
                                text: Files.searching ? "search" : Files.error.length > 0 ? "error_outline" : Files.query.length > 0 ? "filter_list" : "folder_open"
                                size: 28
                                color: Colors.subtle
                            }

                            Text {
                                anchors.horizontalCenter: parent.horizontalCenter
                                text: Files.searching ? (Files.searchQuery.trim().length < 2 ? "Type to search everywhere" : Files.searchBusy ? "Searching…" : "No results") : Files.loading ? "Loading…" : Files.error.length > 0 ? Files.error : Files.query.length > 0 ? "No matches" : "This folder is empty"
                                color: Colors.textMuted
                                font.pixelSize: 12
                                font.family: Fonts.family
                            }
                        }
                    }
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 1
                color: Colors.border
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: false
                Layout.preferredHeight: Files.picker ? 62 : 44
                Layout.leftMargin: 18
                Layout.rightMargin: 14
                spacing: 12

                Text {
                    visible: Files.picker
                    text: Files.title
                    color: Colors.textBright
                    font.pixelSize: 13
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }

                Field {
                    id: nameField

                    visible: Files.save
                    Layout.preferredWidth: 280
                    placeholder: "File name"

                    onTextChanged: Files.fileName = text
                    onAccepted: Files.accept()
                    onEscaped: root.focusList()
                }

                Text {
                    Layout.fillWidth: true
                    text: Files.status
                    color: Files.saveExists && Files.notice.length === 0 ? Colors.yellow : Files.notice.length > 0 ? Colors.textDimmed : Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                    elide: Text.ElideMiddle
                    horizontalAlignment: Files.picker ? Text.AlignRight : Text.AlignLeft
                }

                Text {
                    visible: !Files.picker && Files.clip.paths.length > 0
                    text: `${Files.clip.paths.length} ${Files.clip.op === "cut" ? "to move" : "to paste"} · Ctrl+V`
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                TextButton {
                    visible: Files.picker
                    text: "Cancel"
                    onClicked: Files.cancel()
                }

                TextButton {
                    visible: Files.picker
                    text: Files.action
                    primary: true
                    enabled: Files.canAccept
                    onClicked: Files.accept()
                }
            }
        }

        Rectangle {
            id: ghost

            property bool copy: false

            visible: root.dragging
            width: ghostRow.implicitWidth + 24
            height: 30
            radius: 8
            color: Colors.surfaceActive
            border.width: 1
            border.color: Colors.primary
            opacity: 0.95
            z: 40

            Drag.dragType: Drag.Automatic
            Drag.hotSpot.x: 0
            Drag.hotSpot.y: 0
            Drag.proposedAction: ghost.copy ? Qt.CopyAction : Qt.MoveAction
            Drag.supportedActions: Qt.CopyAction | Qt.MoveAction
            Drag.mimeData: ({ "text/uri-list": Files.uriList(Files.dragPaths), "text/plain": Files.dragPaths.join("\n") })
            Drag.onDragFinished: root.finishDrag()

            Row {
                id: ghostRow
                anchors.centerIn: parent
                spacing: 8

                MaterialIcon {
                    anchors.verticalCenter: parent.verticalCenter
                    text: ghost.copy ? "content_copy" : "open_with"
                    size: 15
                    color: Colors.primary
                }

                Text {
                    anchors.verticalCenter: parent.verticalCenter
                    text: Files.dragPaths.length === 1 ? Files.basename(Files.dragPaths[0]) : `${Files.dragPaths.length} items`
                    color: Colors.textBright
                    font.pixelSize: 12
                    font.family: Fonts.family
                }
            }
        }

        Menu {
            id: menu
            onDismissed: keys.forceActiveFocus()
        }

        Dialog {
            anchors.fill: parent
            onClosed: keys.forceActiveFocus()
        }

        OpenWith {
            anchors.fill: parent
            onClosed: keys.forceActiveFocus()
        }

        Properties {
            anchors.fill: parent
            onClosed: keys.forceActiveFocus()
        }
    }
}
