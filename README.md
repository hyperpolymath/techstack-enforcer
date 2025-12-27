# Techstack Enforcer

A formally verified technology stack filter list enforcement system written in Ada/SPARK. Provides absolute blocking of unwanted technologies across all development layers with real-time notifications and a learning mode that suggests safer language alternatives based on security analysis.

## Features

- **Multi-layer Blocking**: Git hooks, CI/CD pipelines, file watchers, container builds
- **Ada/SPARK TUI**: Fast keyboard-driven terminal interface for filter management
- **Formal Verification**: Core engine verified with SPARK for correctness guarantees
- **Learning Mode**: Analyzes security patterns and suggests memory-safe alternatives
- **Desktop Notifications**: Real-time alerts for violations
- **Flexible Modes**: Learning, Warn-Only, Enforce, Lockdown

## Quick Start

```bash
# Build
gprbuild -P techstack_enforcer.gpr -XMODE=release

# Run TUI
./bin/techstack_tui_main

# Audit a repository
./bin/techstack_main audit /path/to/repo

# Install hooks to a repo
./scripts/install-repo-hooks.sh /path/to/repo
```

## Architecture

```
src/
├── core/
│   ├── techstack_types.ads      # SPARK-verified type definitions
│   ├── techstack_patterns.ads   # Glob pattern matching engine
│   ├── techstack_enforcer.ads   # Core enforcement logic
│   └── techstack_notify.ads     # Notification system
├── tui/
│   └── techstack_tui.ads        # Terminal user interface
├── learning/
│   └── techstack_learning.ads   # Security pattern analyzer
├── hooks/
│   └── techstack_hook.adb       # Git hook handler
├── techstack_main.adb           # CLI entry point
└── techstack_tui_main.adb       # TUI entry point
```

## Enforcement Modes

| Mode | Behavior |
|------|----------|
| **Learning** | Observe and log only, no blocking |
| **Warn** | Show warnings, but allow commits |
| **Enforce** | Block on `Block` and `Fatal` violations |
| **Lockdown** | Block on all violations including `Warn` |

## Filter Levels

| Level | Description |
|-------|-------------|
| `Allow` | Explicitly permitted (whitelist) |
| `Warn` | Warning only, allowed in most modes |
| `Block` | Blocked in Enforce/Lockdown modes |
| `Fatal` | Always blocked (except Learning mode) |

## Default Blocked Technologies

### Fatal (Always Blocked)
- Python (*.py, requirements.txt, etc.)
- PHP (*.php, composer.json)

### Block (Blocked in Enforce Mode)
- C/C++ (*.c, *.cpp) - Use Rust or Ada/SPARK
- JavaScript (*.js) - Use TypeScript or ReScript
- Docker - Use Podman/Containerfile
- Ruby (*.rb)

### Warn
- Shell scripts - Caution for command injection
- C/C++ headers - May indicate unsafe codebase
- GitHub configs - Consider GitLab CI

### Explicitly Allowed
- Rust (*.rs)
- Ada/SPARK (*.adb, *.ads)
- Haskell (*.hs)
- Elixir (*.ex, *.exs)
- Erlang (*.erl)
- OCaml (*.ml)
- ReScript (*.res)
- Elm (*.elm)
- TypeScript (*.ts, *.tsx)
- Nix (*.nix)

## TUI Keyboard Shortcuts

```
Navigation:
  j/k, Up/Down  - Move selection
  g             - Go to first filter
  G             - Go to last filter

Filter Management:
  a             - Add new filter
  d             - Delete selected
  Space         - Toggle enabled/disabled

Enforcement Mode:
  1             - Learning mode
  2             - Warn-only mode
  3             - Enforce mode
  4             - Lockdown mode

Other:
  s             - Scan repository
  S             - View statistics
  r             - Reset counters
  w             - Save configuration
  ?             - Help
  q             - Quit
```

## Learning Mode

The learning system analyzes security vulnerabilities and recommends languages that prevent them by design:

| Vulnerability | Recommended Languages |
|--------------|----------------------|
| Buffer Overflow | Rust, Ada/SPARK, Haskell |
| Null Dereference | Rust, Haskell, ReScript, Elm |
| Race Condition | Elixir, Erlang, Rust |
| Use After Free | Rust, Ada/SPARK |
| Integer Overflow | Ada/SPARK, Rust |
| Type Confusion | Haskell, OCaml, Rust |
| Injection Attacks | Haskell, Rust, ReScript |

## Git Hook Installation

### Per-Repository
```bash
cd /your/repo
ln -s /path/to/techstack/scripts/hooks/pre-commit .git/hooks/
ln -s /path/to/techstack/scripts/hooks/pre-push .git/hooks/
```

### Global (All Repos)
```bash
git config --global core.hooksPath /path/to/techstack/scripts/hooks
```

### Batch Install
```bash
./scripts/install-repo-hooks.sh /path/to/workspace
```

## GitLab CI Integration

Add to your `.gitlab-ci.yml`:

```yaml
include:
  - project: 'your-group/techstack-enforcer'
    file: '/.gitlab-ci.yml'
    ref: main

stages:
  - validate

techstack:
  extends: .techstack-check
```

## Configuration

Edit `~/.config/techstack/techstack.toml` or use the TUI:

```toml
[mode]
enforce = "enforce"

[[block]]
pattern = "*.py"
level = "fatal"
reason = "Python: No static typing, memory-unsafe"
alternatives = ["Rust", "Haskell", "Elixir"]

[[block]]
pattern = "Dockerfile"
level = "block"
reason = "Use Containerfile for Podman"
alternatives = ["Containerfile"]
```

## Building from Source

Requirements:
- GNAT (Ada compiler)
- GPRbuild
- GNATprove (optional, for SPARK verification)

```bash
# Debug build
gprbuild -P techstack_enforcer.gpr -XMODE=debug

# Release build
gprbuild -P techstack_enforcer.gpr -XMODE=release

# SPARK verification
gnatprove -P techstack_enforcer.gpr --level=2
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `TECHSTACK_BIN` | Path to enforcer binary | `techstack-enforcer` |
| `TECHSTACK_MODE` | Enforcement mode | `enforce` |
| `TECHSTACK_NOTIFY` | Enable desktop notifications | `1` |

## Philosophy

This tool enforces a preference for:

1. **Memory-safe languages** (Rust, Ada/SPARK, Haskell) over unsafe ones (C, C++)
2. **Static typing** (TypeScript, Rust) over dynamic typing (JavaScript, Python)
3. **Formal verification** (Ada/SPARK) where correctness is critical
4. **Actor model** (Elixir, Erlang) for concurrent systems
5. **Open infrastructure** (Podman, GitLab) over vendor lock-in (Docker, GitHub)

## License

MIT
