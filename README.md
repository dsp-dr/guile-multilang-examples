# Guile Multilang Examples

Examples demonstrating Guile Scheme interoperability with multiple programming languages through FFI, embeddings, and cross-language integration patterns.

## Quick Start

```bash
# Initialize submodules and run examples
make setup
make test

# Download documentation
make docs
```

## Features

- **Elisp Compilation**: Compile and execute Emacs Lisp within Guile
- **WASM Generation**: Use Hoot to compile Scheme to WebAssembly
- **Cross-Language Interop**: Demonstrations of language integration patterns
- **Performance Benchmarks**: Compare execution across different compilation modes

## Documentation

### Specifications & Manuals

- **R7RS Small Specification**: The standard Scheme language specification
  - Online: https://small.r7rs.org/
  - PDF: `make docs/r7rs.pdf`
  
- **Geiser Manual**: IDE integration for Scheme development
  - Download with GPG verification: `make docs/geiser-0.10.pdf`

### Building from Source

```bash
# Install Hoot locally (for WASM compilation)
make hoot-install

# Build Guile from source
make guile-build

# Build Geiser
make geiser-build
```

## Examples

### Elisp Integration

```bash
make elisp-example
```

Demonstrates:
- Dynamic Elisp compilation
- Function definition and execution
- List processing paradigms
- Cross-language performance comparison

### Hoot WASM Compilation

```bash
# First install Hoot
make hoot-install

# Then compile examples to WASM
make hoot-example
```

Includes:
- Fibonacci implementations (recursive, iterative, memoized, matrix)
- DOM interaction examples
- Interactive web interface

### Cross-Language Demo

```bash
make cross-lang-demo
```

Shows:
- Runtime Elisp compilation within Scheme
- Language paradigm comparisons
- Performance benchmarking

## Development

Start a configured development environment:

```bash
make dev-env
```

This launches tmux with Emacs configured for Scheme development using Geiser.

## Project Structure

```
.
├── elisp/              # Elisp compilation examples
├── examples/           
│   ├── hoot/          # WASM compilation examples
│   ├── cross-language-demo.scm
│   └── performance-comparison.scm
├── submodules/        # Git submodules
│   ├── guile/        # GNU Guile source
│   └── geiser/       # Geiser IDE support
├── vendor/            # Local installations (Hoot)
└── docs/              # Downloaded documentation
```

## Requirements

- GNU Guile 2.2+ (3.0+ recommended for Hoot)
- Git
- Make
- curl or wget
- Optional: Emacs, tmux, GPG

## License

Examples are provided for educational purposes. See individual submodules for their respective licenses.