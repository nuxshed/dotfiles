import Quickshell
import Quickshell.Io
import "../../services"
import "../../services/spotlight"

Scope {
    IpcHandler {
        target: "screenshot"

        function region(mode: string): void {
            Capture.region(mode === "" ? "copy" : mode);
        }

        function fullscreen(): void {
            Capture.fullscreen();
        }

        function colour(): void {
            Capture.pickColor();
        }
    }

    IpcHandler {
        target: "recorder"

        function screen(): void {
            if (Recorder.kind === "screen")
                Recorder.stop();
            else
                Recorder.startScreen("");
        }

        function voice(): void {
            if (Recorder.kind === "voice")
                Recorder.stop();
            else
                Recorder.startVoice();
        }

        function stop(): void {
            Recorder.stop();
        }

        function active(): bool {
            return Recorder.active;
        }
    }

    IpcHandler {
        target: "spotlight"

        function toggle(): void {
            Spotlight.toggle();
        }

        function show(): void {
            Spotlight.show();
        }

        function hide(): void {
            Spotlight.hide();
        }

        function open(text: string): void {
            Spotlight.show();
            Spotlight.setQuery(text);
        }
    }

    IpcHandler {
        target: "preview"

        function open(path: string): void {
            Preview.openFile(path);
        }

        function close(): void {
            Preview.close();
        }
    }

    IpcHandler {
        target: "sysmon"

        function toggle(): void {
            SysMon.toggle();
        }

        function open(tab: string): void {
            SysMon.show(tab);
        }

        function close(): void {
            SysMon.open = false;
        }

        function maintenance(): void {
            SysMon.openMaintenance();
        }

        function storage(path: string): void {
            SysMon.show("storage");
            if (path.endsWith("//"))
                SysMon.showFiles(path.slice(0, -2));
            else
                SysMon.showDir(path);
        }
    }

    IpcHandler {
        target: "files"

        function request(multiple: string, directory: string, save: string, path: string, out: string): void {
            Files.request(multiple, directory, save, path, out);
        }

        function show(): void {
            Files.show();
        }

        function toggle(): void {
            if (Files.open)
                Files.hide();
            else
                Files.show();
        }

        function browse(path: string): void {
            Files.browse(path);
        }

        function recent(): void {
            Files.browse(Files.cwd);
            Files.showRecent();
        }

        function cancel(): void {
            Files.cancel();
        }

        function pending(): string {
            return Files.outFile;
        }
    }

    IpcHandler {
        target: "lock"

        function lock(): void {
            Lock.lock();
        }

        function locked(): bool {
            return Lock.locked;
        }

        function mode(name: string): void {
            Lock.setMode(name);
        }
    }

    IpcHandler {
        target: "notes"

        function toggle(): void {
            Notes.toggle();
        }

        function open(): void {
            Notes.open = true;
        }

        function hide(): void {
            Notes.open = false;
        }

        function add(): void {
            Notes.add();
        }
    }

    IpcHandler {
        target: "booth"

        function toggle(): void {
            Booth.toggle();
        }

        function open(): void {
            Booth.show();
        }

        function close(): void {
            Booth.close();
        }

        function shoot(): void {
            Booth.show();
            Booth.shoot();
        }

        function effects(): void {
            Booth.show();
            Booth.picking = !Booth.picking;
        }
    }

    IpcHandler {
        target: "overview"

        function toggle(): void {
            Overview.toggle();
        }

        function open(tab: string): void {
            Overview.show(tab);
        }

        function close(): void {
            Overview.open = false;
        }
    }

    IpcHandler {
        target: "pomodoro"

        function toggle(): void {
            Pomodoro.toggle();
        }

        function start(): void {
            Pomodoro.start();
        }

        function swap(): void {
            Pomodoro.swap();
        }

        function stop(): void {
            Pomodoro.end();
        }
    }

    IpcHandler {
        target: "focus"

        function toggle(): void {
            Pomodoro.appOpen = !Pomodoro.appOpen;
        }

        function open(): void {
            Pomodoro.appOpen = true;
        }

        function close(): void {
            Pomodoro.appOpen = false;
        }
    }

    IpcHandler {
        target: "agenda"

        function add(url: string): void {
            Agenda.add(url);
        }

        function refresh(): void {
            Agenda.refresh();
        }
    }

    IpcHandler {
        target: "tasks"

        function add(text: string): void {
            Tasks.add(text);
        }
    }

    IpcHandler {
        target: "calendar"

        function toggle(): void {
            Calendar.open = !Calendar.open;
        }

        function open(): void {
            Calendar.show(new Date());
        }

        function close(): void {
            Calendar.open = false;
        }

        function view(name: string): void {
            Calendar.view = name;
            Calendar.open = true;
        }

        function add(title: string): void {
            const d = new Date();
            d.setMinutes(0, 0, 0);
            d.setHours(d.getHours() + 1);
            Calendar.show(d);
            Calendar.draft(d.getTime(), false);
            Calendar.editing = Object.assign({}, Calendar.editing, { summary: title });
        }
    }

    IpcHandler {
        target: "timers"

        function stopwatch(): void {
            Timers.stopwatch();
        }

        function start(minutes: string): void {
            Timers.timer(parseFloat(minutes) || 5);
        }
    }

    IpcHandler {
        target: "switcher"

        function next(mode: string): void {
            Switcher.step(mode || "windows", 1);
        }

        function prev(mode: string): void {
            Switcher.step(mode || "windows", -1);
        }

        function commit(): void {
            Switcher.commit();
        }

        function cancel(): void {
            Switcher.cancel();
        }
    }

    IpcHandler {
        target: "emoji"

        function toggle(): void {
            Emoji.toggle();
        }
    }
}
