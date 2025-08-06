# Submodules Overview

This repository includes several important submodules that demonstrate Guile's ecosystem and multilanguage capabilities.

## Core Language Implementation

### GNU Guile (`submodules/guile`)
- **Source**: https://git.savannah.gnu.org/git/guile.git
- **Version**: v3.0.10-214-g755f703dc
- **Purpose**: The main Scheme implementation with multilanguage support
- **Features**: Elisp, ECMAScript support, bytecode VM, native compilation

## Development Tools

### Geiser (`submodules/geiser`)
- **Source**: https://gitlab.com/emacs-geiser/geiser.git
- **Version**: 0.31.1-7-gc1c2707
- **Purpose**: Generic Emacs/Scheme interaction mode
- **Features**: REPL integration, documentation, debugging, completion

### Geiser Guile Backend (`submodules/geiser-guile`)
- **Source**: https://gitlab.com/emacs-geiser/guile.git
- **Version**: 0.28.3
- **Purpose**: Guile-specific backend for Geiser
- **Features**: Guile REPL integration, module introspection, debugging

### Guile Ares RS (`submodules/guile-ares-rs`)
- **Source**: https://git.sr.ht/~abcdw/guile-ares-rs
- **Version**: 0.9.6-47-gacf339f
- **Purpose**: Asynchronous RPC server implementing nREPL protocol
- **Features**: 
  - Async evaluation and code exploration
  - Interruptible execution
  - IDE and tooling integration
  - Foundation for LSP servers
  - Works with Arei IDE

## Editor Support

### GNU Emacs (`submodules/emacs`)
- **Source**: https://git.savannah.gnu.org/git/emacs.git
- **Version**: emacs-30.1-178560-gf0da94e2c28
- **Purpose**: Reference for Elisp implementation and integration
- **Features**: Native Elisp support, Guile integration possibilities

## Usage

### Initialize All Submodules
```bash
git submodule update --init --recursive
```

### Update All Submodules
```bash
git submodule update --remote --merge
```

### Using repo-sync
The repository is configured to work with `repo-sync` for automatic updates:
```bash
~/bin/repo-sync
```

## Integration Examples

### Geiser Setup
```elisp
(require 'geiser)
(setq geiser-guile-binary "guile3")
(setq geiser-active-implementations '(guile))
```

### Ares RS Server
```bash
# Start nREPL server
guile -L submodules/guile-ares-rs/modules \
      -c "(use-modules (ares server)) (run-nrepl-server)"
```

### Development Workflow
1. Use Geiser for interactive development
2. Run Ares RS for advanced tooling support
3. Compile to WASM with Hoot (when available)
4. Test Elisp integration with included examples

## Debugging Tools

### GDB Init (`submodules/gdb-init`)
- **Source**: https://github.com/gdbinit/Gdbinit.git
- **Purpose**: Enhanced GDB initialization and configuration
- **Features**: Better formatting, colored output, enhanced commands

### GDB Dashboard (`submodules/gdb-dashboard`)
- **Source**: https://github.com/cyrus-and/gdb-dashboard.git
- **Purpose**: Modern dashboard interface for GDB
- **Features**: 
  - Python-based modular interface
  - Real-time source code view
  - Register and memory visualization
  - Stack frame navigation

### GNU Debugger Full Source (`submodules/gdb`)
- **Source**: https://sourceware.org/git/binutils-gdb.git
- **Purpose**: Complete GDB source for custom builds
- **Features**: Latest GDB features, custom compilation support

## Development Environment Integrations

### Magit Forge (`submodules/forge`)
- **Source**: https://github.com/magit/forge.git
- **Purpose**: GitHub/GitLab integration for Emacs
- **Features**:
  - Issue tracking and management
  - Pull request creation and review
  - Repository browsing and administration
  - Integration with Magit workflows
  - Support for GitHub, GitLab, Codeberg, SourceHut

### Claude Code IDE (`submodules/claude-code-ide`)
- **Source**: https://github.com/manzaltu/claude-code-ide.el.git
- **Purpose**: AI-powered code assistance for Emacs
- **Features**:
  - Code explanation and documentation
  - Intelligent refactoring suggestions
  - Code generation from natural language
  - Context-aware programming assistance
  - Integration with Claude AI models

## Integration Workflow

### Combined Development Environment
```bash
# Setup all integrations
./setup-emacs-integrations.sh

# Launch configured Emacs
./emacs-launch.sh
```

### Available Commands
- **Guile Development**: `C-c g r` (REPL), `C-c g e` (examples)
- **Git Integration**: `C-c g i` (issues), `C-c g p` (pull requests)
- **AI Assistance**: `C-c c e` (explain), `C-c c r` (refactor)
- **Debugging**: GDB integration with enhanced interfaces

## Notes
- All submodules are in active development
- Some features require bleeding-edge Guile versions
- Integration examples demonstrate cross-language capabilities
- GDB tools enhance debugging of compiled Guile code
- Emacs integrations provide comprehensive development workflow
- AI assistance helps with complex Scheme and multilang development