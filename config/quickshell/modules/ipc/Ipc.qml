import Quickshell
import Quickshell.Io
import "../../services"

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
        target: "lock"

        function lock(): void {
            Lock.lock();
        }

        function locked(): bool {
            return Lock.locked;
        }
    }
}
