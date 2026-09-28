pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    function ask(fifo: string, title: string, desc: string, prompt: string, error: string, mode: string): void {
        const confirm = mode === "confirm";
        Prompt.ask({
            title: title || (confirm ? "Confirm" : "Password required"),
            subtitle: desc,
            placeholder: prompt.replace(/:\s*$/, "") || "Password",
            action: confirm ? "OK" : "Unlock",
            error: error,
            inputless: confirm,
            onSubmit: value => root.reply(fifo, "OK " + value),
            onCancel: () => root.reply(fifo, "CANCEL")
        });
    }

    function reply(target: string, data: string): void {
        const p = writer.createObject(root, { payload: data });
        p.command = ["sh", "-c", 'exec cat > "$1"', "sh", target];
        p.running = true;
    }

    Component {
        id: writer

        Process {
            id: proc

            property string payload: ""

            stdinEnabled: true
            onStarted: {
                proc.write(proc.payload + "\n");
                proc.payload = "";
                proc.stdinEnabled = false;
            }
            onExited: proc.destroy()
        }
    }
}
