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
}
