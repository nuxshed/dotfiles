pragma Singleton

import QtQuick
import Quickshell

Singleton {
    id: root

    readonly property var boundary: /[\s\-_./\\:]/

    function isBoundary(ch) {
        return root.boundary.test(ch)
    }

    function subsequence(q, t) {
        let ti = 0
        let bonus = 0
        let streak = 0

        for (let qi = 0; qi < q.length; qi++) {
            let found = -1
            for (let k = ti; k < t.length; k++) {
                if (t.charAt(k) === q.charAt(qi)) {
                    found = k
                    break
                }
            }
            if (found < 0)
                return -1

            if (found === ti && qi > 0) {
                streak++
                bonus += 0.6 + Math.min(streak, 4) * 0.1
            } else {
                streak = 0
                bonus += (found === 0 || root.isBoundary(t.charAt(found - 1))) ? 0.9 : 0.35
            }
            ti = found + 1
        }

        return Math.min(0.62, 0.2 + (bonus / q.length) * 0.45)
    }

    function levenshtein(a, b, max) {
        const al = a.length
        const bl = b.length
        if (Math.abs(al - bl) > max)
            return max + 1

        let prev = []
        for (let j = 0; j <= bl; j++)
            prev.push(j)

        for (let i = 1; i <= al; i++) {
            const cur = [i]
            let best = i
            for (let j = 1; j <= bl; j++) {
                const cost = a.charAt(i - 1) === b.charAt(j - 1) ? 0 : 1
                let v = Math.min(prev[j] + 1, cur[j - 1] + 1, prev[j - 1] + cost)
                if (i > 1 && j > 1 && a.charAt(i - 1) === b.charAt(j - 2) && a.charAt(i - 2) === b.charAt(j - 1))
                    v = Math.min(v, prev[j - 2] !== undefined ? prev[j - 2] + 1 : v)
                cur.push(v)
                if (v < best)
                    best = v
            }
            if (best > max)
                return max + 1
            prev = cur
        }
        return prev[bl]
    }

    function typo(q, t) {
        if (q.length < 4)
            return -1

        const max = q.length >= 7 ? 2 : 1
        const words = t.split(root.boundary).filter(w => w.length > 0)

        for (const w of words) {
            if (root.levenshtein(q, w, max) <= max)
                return 0.5
            if (w.length > q.length && root.levenshtein(q, w.substring(0, q.length), max) <= max)
                return 0.42
        }
        return -1
    }

    function score(query, target) {
        if (!target)
            return -1

        const q = query.toLowerCase().trim()
        const t = target.toLowerCase()
        if (!q)
            return 0.5

        const penalty = Math.min(0.12, Math.max(0, t.length - q.length) / 300)

        if (t === q)
            return 1
        const idx = t.indexOf(q)
        if (idx === 0)
            return 0.95 - penalty
        if (idx > 0)
            return (root.isBoundary(t.charAt(idx - 1)) ? 0.86 : 0.68) - penalty

        const seq = root.subsequence(q, t)
        if (seq >= 0)
            return seq - penalty

        return root.typo(q, t) - penalty
    }

    function best(query, targets) {
        let top = -1
        for (const t of targets) {
            const s = root.score(query, t)
            if (s > top)
                top = s
        }
        return top
    }
}
