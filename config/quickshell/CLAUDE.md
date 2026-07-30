# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## UI Style
Simple, Clean, Minimal UI
Make sure to stay consistent with current style.

## Code Style
Simple clean concise code
No comments in code
Ensure correct quickshell code, refer to documentation of quickshell/qml if needed. also refer to caelestia dots in .llms/

## Running / Reloading

This is a [Quickshell](https://git.outfoxxed.me/outfoxxed/quickshell) config — QML is interpreted at runtime, no build step needed.

```bash
# Restart the shell to apply changes
pkill quickshell

# Launch manually (if not started by Hyprland)
quickshell -p ~/.config/quickshell
```

The config directory is symlinked into `~/.config/quickshell` via home-manager. To rebuild NixOS/home-manager (e.g., to update the quickshell package itself):

```bash
sudo nixos-rebuild switch --flake ~/dotfiles#zephyrus
```

## Architecture

**Entry point:** `shell.qml` — a `Scope` that instantiates all top-level modules.

### Layer structure

```
shell.qml
├── modules/leftpanel/   — Vertical left-edge panel (blocks + popups)
├── modules/notifications/ — Notification popups + right-side history sidepanel
└── modules/osd/         — Volume and brightness overlays (bottom-center)
```

### Services (global singletons)

All files under `services/` are QML singletons — they provide system data and are imported directly by name anywhere in the codebase:

| Singleton | Source | What it provides |
|---|---|---|
| `Battery` | `acpi -b` polled every 5s | Level, charging state, time remaining |
| `Network` | `nmcli` | WiFi SSID, signal strength, scan/connect |
| `Mpris` | D-Bus MPRIS | Media player state, metadata, controls |
| `HyprlandData` | `hyprctl` JSON | Workspaces, clients, monitors |
| `Asusctl` | `asusctl` | Power profiles, keyboard LED/aura (ASUS-specific) |
| `Notifications` | D-Bus notification server | Notification list with auto-dismiss |
| `Bluetooth` | `bluetoothctl` | BT devices, connect/disconnect |

### Panel/window types

- **`PanelWindow`** — persistent anchored panels (LeftPanel, NotificationSidepanel)
- **`PopupWindow`** — transient contextual menus (NetworkPopup, MediaPopup, Power)
- **`LazyLoader`** — OSD overlays loaded on demand

### Colors

All colors are defined in `config/Colors.qml` (singleton). Reference as `Colors.background`, `Colors.accent`, etc. Do not hardcode color values in components.

### Multi-screen

Panels that need per-screen instances use `Variants { model: Quickshell.screens }` with `screen: modelData` on the window.

## Reference material

`.llms/` contains reference QML files from [caelestia-dots/shell](https://github.com/caelestia-dots/shell) used as inspiration. These are not part of the active shell config.

## Key Quickshell imports

```qml
import Quickshell              // Scope, Variants, LazyLoader
import Quickshell.Wayland      // WlrLayershell
import Quickshell.Widgets      // PanelWindow, PopupWindow
import Quickshell.Hyprland     // Hyprland IPC integration
import Quickshell.Io           // Process, IpcHandler
import Quickshell.Services.Pipewire  // Audio
import Quickshell.Services.Notifications  // D-Bus notifications
```
