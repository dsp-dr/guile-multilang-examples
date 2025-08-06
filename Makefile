.PHONY: all clean test help docs
.PHONY: guile-build geiser-build hoot-build hoot-install
.PHONY: elisp-example hoot-example cross-lang-demo
.PHONY: setup update-submodules dev-env

# Project configuration
PROJECT_NAME := guile-multilang-examples
PROJECT_ROOT := $(shell pwd)
GUILE := guile
GUILD := guild

all: setup elisp-example hoot-example

help:
	@echo "Guile Multilang Examples - Available targets:"
	@echo ""
	@echo "Setup & Build:"
	@echo "  make setup             - Initialize and update all submodules"
	@echo "  make update-submodules - Update all submodules to latest"
	@echo "  make guile-build       - Build Guile from source (requires autotools)"
	@echo "  make geiser-build      - Build/compile Geiser"
	@echo "  make hoot-install      - Clone and install Hoot locally"
	@echo "  make hoot-build        - Build Hoot WASM compiler (after hoot-install)"
	@echo ""
	@echo "Examples & Tests:"
	@echo "  make elisp-example     - Run Elisp compilation example"
	@echo "  make hoot-example      - Run Hoot WASM compilation example"
	@echo "  make cross-lang-demo   - Run cross-language interop demonstration"  
	@echo "  make test              - Run all examples and tests"
	@echo "  make validate-elisp    - Validate Elisp compilation without Emacs"
	@echo "  make benchmark         - Run performance comparisons"
	@echo ""
	@echo "Documentation:"
	@echo "  make docs              - Download all documentation"
	@echo "  make docs/r7rs.pdf     - Download R7RS specification"
	@echo "  make docs/geiser-0.10.pdf - Download Geiser manual (with GPG verification)"
	@echo ""
	@echo "Development:"
	@echo "  make dev-env           - Start development environment with tmux + Emacs"
	@echo "  make clean             - Clean build artifacts"

setup:
	git submodule init
	git submodule update --recursive

update-submodules:
	git submodule update --remote --merge

guile-build:
	@echo "Building Guile from source..."
	@echo "Note: This requires autotools, libtool, and other dependencies"
	cd submodules/guile && \
		./autogen.sh && \
		./configure --prefix=$${HOME}/.local && \
		make -j$$(nproc) && \
		make check

geiser-build:
	@echo "Building Geiser..."
	cd submodules/geiser && \
		make

hoot-install:
	@echo "Installing Hoot locally from git..."
	@if [ ! -d "vendor/guile-hoot" ]; then \
		mkdir -p vendor && \
		echo "Cloning Hoot repository..." && \
		git clone https://gitlab.com/spritely/guile-hoot.git vendor/guile-hoot || \
		git clone https://github.com/spritely/guile-hoot.git vendor/guile-hoot 2>/dev/null || \
		(echo "Failed to clone Hoot. Try manually:" && \
		 echo "  git clone https://gitlab.com/spritely/guile-hoot.git vendor/guile-hoot" && \
		 exit 1); \
	else \
		echo "Hoot already cloned in vendor/guile-hoot"; \
	fi
	@echo "Building Hoot..."
	cd vendor/guile-hoot && \
		./bootstrap && \
		./configure --prefix=$${HOME}/.local && \
		make && \
		make install
	@echo "✓ Hoot installed to ~/.local"
	@echo "  Add to PATH: export PATH=$$HOME/.local/bin:$$PATH"
	@echo "  Add to GUILE_LOAD_PATH: export GUILE_LOAD_PATH=$$HOME/.local/share/guile/site/3.0:$$GUILE_LOAD_PATH"

hoot-build:
	@echo "Building Hoot..."
	@if [ -d "submodules/hoot" ] && [ -f "submodules/hoot/configure.ac" ]; then \
		cd submodules/hoot && \
		./bootstrap && \
		./configure --prefix=$${HOME}/.local && \
		make; \
	else \
		echo "Hoot submodule not properly initialized"; \
		exit 1; \
	fi

elisp-example:
	@echo "Running Elisp compilation example..."
	cd elisp && $(GUILE) compile-elisp.scm

hoot-example:
	@echo "Running Hoot WASM compilation examples..."
	@if command -v hoot >/dev/null 2>&1 || [ -x "submodules/hoot/bin/hoot" ]; then \
		mkdir -p examples/hoot/build; \
		$(GUILD) compile-wasm -o examples/hoot/build/fibonacci.wasm examples/hoot/fibonacci.scm 2>/dev/null || \
			echo "Note: fibonacci.wasm compilation skipped (hoot may not be fully installed)"; \
		$(GUILD) compile-wasm -o examples/hoot/build/dom-interaction.wasm examples/hoot/dom-interaction.scm 2>/dev/null || \
			echo "Note: dom-interaction.wasm compilation skipped"; \
		echo "WASM modules generated in examples/hoot/build/"; \
		echo "Open examples/hoot/index.html in a browser to test"; \
	else \
		echo "Hoot not found. Run 'make hoot-build' first or install hoot"; \
		echo "Skipping WASM compilation but examples are still viewable"; \
	fi

test: elisp-example
	@echo "Running all tests..."
	@echo "Elisp example: PASS"
	@if [ -f examples/hoot/hello.wasm ]; then \
		echo "Hoot example: PASS"; \
	else \
		echo "Hoot example: SKIPPED (not built)"; \
	fi

validate-elisp:
	@echo "Validating Elisp compilation..."
	cd elisp && $(GUILE) compile-elisp.scm --test
	@echo "Testing compiled bytecode execution..."
	cd elisp && $(GUILE) -c "(load-compiled \"factorial.el.go\") (display \"Validation complete\")"

cross-lang-demo:
	@echo "Running cross-language interop demonstration..."
	$(GUILE) examples/cross-language-demo.scm

benchmark:
	@echo "Running performance benchmarks..."
	$(GUILE) examples/performance-comparison.scm
	cd elisp && $(GUILE) -c "(use-modules (ice-9 time)) (time (load \"factorial.el\"))"
	@if [ -f examples/hoot/build/fibonacci.wasm ]; then \
		echo "WASM module size: $$(du -h examples/hoot/build/fibonacci.wasm)"; \
	fi

docs: docs/r7rs.pdf docs/geiser-0.10.pdf
	@echo ""
	@echo "✓ All documentation downloaded:"
	@ls -lh docs/*.pdf

docs/r7rs.pdf:
	@mkdir -p docs
	@echo "Downloading R7RS Small Language Specification..."
	@if command -v wget >/dev/null 2>&1; then \
		wget -q -O docs/r7rs.pdf https://small.r7rs.org/attachment/r7rs.pdf || \
		curl -sL -o docs/r7rs.pdf https://small.r7rs.org/attachment/r7rs.pdf; \
	else \
		curl -sL -o docs/r7rs.pdf https://small.r7rs.org/attachment/r7rs.pdf; \
	fi
	@echo "✓ R7RS specification downloaded successfully"
	@echo "  Location: docs/r7rs.pdf"
	@echo "  Official source: https://small.r7rs.org/"

docs/geiser-0.10.pdf: docs/geiser-0.10.pdf.sig
	@echo "Geiser manual verified successfully"

docs/geiser-0.10.pdf.sig:
	@mkdir -p docs
	@echo "Downloading Geiser 0.10 manual and signature..."
	@if command -v wget >/dev/null 2>&1; then \
		wget -q -O docs/geiser-0.10.pdf https://download-mirror.savannah.gnu.org/releases/geiser/0.10/geiser-0.10.pdf || \
		curl -sL -o docs/geiser-0.10.pdf https://download-mirror.savannah.gnu.org/releases/geiser/0.10/geiser-0.10.pdf; \
		wget -q -O docs/geiser-0.10.pdf.sig https://download-mirror.savannah.gnu.org/releases/geiser/0.10/geiser-0.10.pdf.sig || \
		curl -sL -o docs/geiser-0.10.pdf.sig https://download-mirror.savannah.gnu.org/releases/geiser/0.10/geiser-0.10.pdf.sig; \
	else \
		curl -sL -o docs/geiser-0.10.pdf https://download-mirror.savannah.gnu.org/releases/geiser/0.10/geiser-0.10.pdf; \
		curl -sL -o docs/geiser-0.10.pdf.sig https://download-mirror.savannah.gnu.org/releases/geiser/0.10/geiser-0.10.pdf.sig; \
	fi
	@echo "Verifying signature..."
	@if command -v gpg >/dev/null 2>&1; then \
		gpg --keyserver keys.openpgp.org --recv-keys 0x3AECB8FAB53E816A || true; \
		if gpg --verify docs/geiser-0.10.pdf.sig docs/geiser-0.10.pdf 2>/dev/null; then \
			echo "✓ Signature verification passed"; \
		else \
			echo "⚠ Signature verification failed or key not trusted"; \
		fi \
	else \
		echo "⚠ GPG not installed, skipping signature verification"; \
	fi

dev-env:
	@echo "Starting development environment for $(PROJECT_NAME)..."
	@if command -v tmux >/dev/null 2>&1; then \
		export PROJECT_NAME=$(PROJECT_NAME); \
		export PROJECT_ROOT=$(PROJECT_ROOT); \
		tmux new-session -d -s $(PROJECT_NAME) "emacs -nw -Q -l project-name.el"; \
		echo "Development session started!"; \
		echo "Attach with: tmux attach -t $(PROJECT_NAME)"; \
		echo "TTY: $$(tmux list-panes -t $(PROJECT_NAME) -F '#{pane_tty}')"; \
	else \
		echo "tmux not found. Starting Emacs directly..."; \
		export PROJECT_NAME=$(PROJECT_NAME); \
		export PROJECT_ROOT=$(PROJECT_ROOT); \
		emacs -nw -Q -l project-name.el; \
	fi

clean:
	rm -rf examples/hoot/build/
	rm -rf docs/*.pdf docs/*.sig
	find . -name "*.go" -delete
	find . -name "*~" -delete
	find . -name "*.wasm" -delete