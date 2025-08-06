#!/bin/sh
# Try to install and use Hoot with system Guile 3.0
# This might work for basic features even without bleeding-edge Guile

set -e

echo "========================================="
echo "Attempting Hoot installation with Guile 3.0"
echo "========================================="
echo ""

# Check for Guile 3
if ! command -v guile3 >/dev/null 2>&1; then
    echo "Error: Guile 3 not found. Install with: pkg install guile3"
    exit 1
fi

echo "Using Guile version:"
guile3 --version | head -1
echo ""

# Create a temporary installation directory
HOOT_PREFIX="${HOME}/.local/hoot-guile3"
mkdir -p "${HOOT_PREFIX}"

# Clone Hoot if not already present
if [ ! -d "vendor/guile-hoot" ]; then
    echo "Cloning Hoot repository..."
    mkdir -p vendor
    git clone --depth 1 https://gitlab.com/spritely/guile-hoot.git vendor/guile-hoot || {
        echo "Failed to clone from GitLab, trying GitHub mirror..."
        git clone --depth 1 https://github.com/spritely/guile-hoot.git vendor/guile-hoot
    }
fi

cd vendor/guile-hoot

echo "Bootstrapping Hoot..."
if [ -f "./bootstrap.sh" ]; then
    ./bootstrap.sh
elif [ -f "./bootstrap" ]; then
    ./bootstrap
else
    autoreconf -vif
fi

echo "Configuring Hoot with Guile 3..."
GUILE=guile3 ./configure --prefix="${HOOT_PREFIX}" || {
    echo ""
    echo "Configuration failed. This might mean Hoot requires newer Guile features."
    echo "You can try:"
    echo "  1. Build Guile from source: make guile-next"
    echo "  2. Use Docker: docker-compose up hoot-dev"
    exit 1
}

echo "Building Hoot..."
make -j$(nproc) || {
    echo ""
    echo "Build failed. Hoot likely requires bleeding-edge Guile."
    echo "Falling back options:"
    echo "  1. Build Guile from source: make guile-next"
    echo "  2. Use Docker: docker-compose up hoot-dev"
    exit 1
}

echo "Installing Hoot..."
make install

echo ""
echo "========================================="
echo "Hoot installation complete!"
echo "========================================="
echo ""
echo "To use Hoot, add to your shell configuration:"
echo "  export PATH=\"${HOOT_PREFIX}/bin:\$PATH\""
echo "  export GUILE_LOAD_PATH=\"${HOOT_PREFIX}/share/guile/site/3.0:\$GUILE_LOAD_PATH\""
echo "  export GUILE_LOAD_COMPILED_PATH=\"${HOOT_PREFIX}/lib/guile/3.0/site-ccache:\$GUILE_LOAD_COMPILED_PATH\""
echo ""
echo "Test with:"
echo "  guild compile-wasm examples/hoot/fibonacci.scm"