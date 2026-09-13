import QtQuick

QtObject {
    id: root

    property string name: ""
    property string label: ""
    property string prefix: ""
    property bool mixed: true
    property real weight: 1
    property int cap: 0
    property var results: []

    function search(text) {
    }

    function clear() {
        root.results = []
    }
}
