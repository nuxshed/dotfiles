import QtQuick
import Quickshell
import "../../../services"
import ".."

Provider {
    id: root

    name: "todos"
    label: "Tasks"
    prefix: "t"
    icon: "check"
    keywords: ["tasks", "todo", "todos", "to-do"]
    weight: 1
    cap: 3

    function taskText(q, scoped) {
        const m = q.match(/^(?:add\s+)?(?:a\s+)?(?:todo|to-do|task)s?\s*:?\s+(.+)$/i) ?? q.match(/^remind\s+me\s+(?:to|about)\s+(.+)$/i)
        if (m)
            return m[1].trim()
        return scoped ? q : ""
    }

    function search(text, scoped) {
        const q = text.trim()
        const out = []
        const add = root.taskText(q, scoped)
        const existing = Tasks.open.find(t => t.text.toLowerCase() === add.toLowerCase())

        if (add && !existing && !(scoped && Tasks.open.some(t => Fuzzy.score(add, t.text) >= 0.9)))
            out.push({
                key: "task:new",
                kind: "todos",
                section: "Tasks",
                icon: "add",
                title: "Add task “" + add + "”",
                subtitle: Tasks.open.length + " open " + (Tasks.open.length === 1 ? "task" : "tasks"),
                score: scoped ? 0.9 : 1.35,
                pinned: true,
                activate: () => Tasks.add(add)
            })

        const listing = scoped || /^(todos?|tasks?|to-dos?)$/i.test(q)
        if (listing || (q.length >= 3 && !add)) {
            const filter = listing ? (scoped ? q : "") : q
            Tasks.open.forEach((t, i) => {
                const s = filter ? Fuzzy.score(filter, t.text) : 0.8 - i * 0.001
                if (s < (listing ? 0 : 0.8))
                    return
                out.push({
                    key: "task:" + t.id,
                    kind: "todos",
                    section: "Open tasks",
                    icon: "radio_button_unchecked",
                    title: t.text,
                    subtitle: "Task",
                    score: s,
                    pinned: true,
                    primary: "Mark done",
                    activate: () => Tasks.toggle(t.id),
                    altActivate: () => Tasks.remove(t.id),
                    altHint: "Delete task",
                    altIcon: "delete",
                    copy: t.text
                })
            })
        }

        root.results = out
    }
}
