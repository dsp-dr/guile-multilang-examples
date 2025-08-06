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

## Notes
- All submodules are in active development
- Some features require bleeding-edge Guile versions
- Integration examples demonstrate cross-language capabilities