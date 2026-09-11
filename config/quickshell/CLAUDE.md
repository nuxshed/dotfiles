## Project
Quickshell (QML) config for a Hyprland rice, managed via home-manager on NixOS.

## Quickshell MCP
This repo has the `quickshell-mcp` server connected. Use it instead of guessing from training data — Quickshell's API moves fast and outdated QML is a common failure mode.

**Before writing new QML:**
- `quickshell_search_all` / `quickshell_find_pattern` — check how similar components are done (Caelestia/Noctalia/dots-hyprland patterns)
- `quickshell_check_compatibility` — confirm an API exists in the Quickshell version this config targets

**After writing QML:**
- `quickshell_validate_qml` — run it before telling me it's done. Fix reported errors before handing back code.

**When something breaks:**
- `quickshell_explain_error` — use this first instead of guessing at the fix

**For a new component from scratch:**
- `quickshell_generate_component` can produce a validated starting point — prefer this over freehand generation for new widgets (bars, OSDs, popups)

**For multi-step work** (build a whole feature, migrate a version, debug something non-obvious):
- `quickshell_coding_assistant` — routes through the right tools automatically, cheaper than doing it manually

Never skip validation to save time. Broken QML that "looks right" wastes more time than the extra tool call.

## UI Style
Simple, clean, minimal. Stay consistent with the current visual style — don't introduce new colors, spacing, or effects without asking.

## Code Style
- Simple, clean, concise QML
- No comments in code
- Refer to `.llms/` (misc quickshell configs) for style/pattern reference when asked to
- When unsure of correct Quickshell/QML usage, check via the MCP tools above rather than assuming
