#!/usr/bin/env bash
# Build Guile from main branch for Hoot compatibility

set -e

GUILE_SRC="$(pwd)/submodules/guile"
GUILE_PREFIX="${HOME}/.local/guile-main"
JOBS=$(nproc || echo 4)

echo "=== Building Guile from main branch ==="
echo "Source: $GUILE_SRC"
echo "Target: $GUILE_PREFIX"
echo "Version: $(cd "$GUILE_SRC" && git describe --tags)"
echo

# Check prerequisites
check_deps() {
    local missing=()
    for cmd in autoconf automake libtool pkg-config; do
        if ! command -v $cmd >/dev/null 2>&1; then
            missing+=($cmd)
        fi
    done
    
    if [ ${#missing[@]} -gt 0 ]; then
        echo "Error: Missing required tools: ${missing[*]}"
        echo "Install with: pkg install ${missing[*]}"
        exit 1
    fi
}

echo "Checking dependencies..."
check_deps

# Check for required libraries
echo "Checking libraries..."
pkg-config --exists bdw-gc || echo "Warning: libgc not found"
pkg-config --exists libffi || echo "Warning: libffi not found"

cd "$GUILE_SRC"

# Switch to stable release for better compatibility
echo "Switching to stable v3.0.10 release..."
git checkout v3.0.10

# Clean any previous build
if [ -f Makefile ]; then
    echo "Cleaning previous build..."
    make distclean || true
fi

# Generate configure script
echo "Running autogen.sh..."
# Use GNU m4 on FreeBSD
export M4=gm4
./autogen.sh

# Apply FreeBSD ports patches
echo "Applying FreeBSD compatibility patches..."

# Apply patch for gen-scmconfig.c (off_t types)
patch -p0 << 'EOF'
--- libguile/gen-scmconfig.c.orig
+++ libguile/gen-scmconfig.c
@@ -329,6 +329,10 @@
   pf ("typedef int scm_t_off;\n");
   pf ("#define SCM_T_OFF_MAX INT_MAX\n");
   pf ("#define SCM_T_OFF_MIN INT_MIN\n");
+#elif SIZEOF_OFF_T == SIZEOF_LONG_LONG && SIZEOF_OFF_T != SIZEOF_LONG
+  pf ("typedef long long int scm_t_off;\n");
+  pf ("#define SCM_T_OFF_MAX LLONG_MAX\n");
+  pf ("#define SCM_T_OFF_MIN LLONG_MIN\n");
 #else
   pf ("typedef long int scm_t_off;\n");
   pf ("#define SCM_T_OFF_MAX LONG_MAX\n");
EOF

# Configure with optimizations for Hoot
echo "Configuring Guile..."
./configure \
    --prefix="$GUILE_PREFIX" \
    --enable-mini-gmp \
    --enable-silent-rules \
    --disable-deprecated \
    --with-threads \
    --with-modules \
    --with-bdw-gc=bdw-gc-threaded \
    CFLAGS="-O2 -g" \
    CPPFLAGS="-I/usr/local/include" \
    LDFLAGS="-L/usr/local/lib" \
    || {
        echo "Configuration failed. Check config.log for details."
        exit 1
    }

# Build
echo "Building Guile (using $JOBS parallel jobs)..."
gmake -j$JOBS || make -j$JOBS || {
    echo "Build failed. Trying single-threaded build..."
    gmake || make
}

# Run tests (optional)
read -p "Run test suite? (y/n) [n]: " run_tests
if [[ $run_tests == [Yy]* ]]; then
    echo "Running tests..."
    gmake check || make check || echo "Some tests failed"
fi

# Install
echo "Installing to $GUILE_PREFIX..."
gmake install || make install

# Update paths
echo
echo "=== Build Complete! ==="
echo
echo "To use this Guile version, add to your shell configuration:"
echo
echo "  export PATH=\"$GUILE_PREFIX/bin:\$PATH\""
echo "  export GUILE_LOAD_PATH=\"$GUILE_PREFIX/share/guile/site/3.0:\$GUILE_LOAD_PATH\""
echo "  export GUILE_LOAD_COMPILED_PATH=\"$GUILE_PREFIX/lib/guile/3.0/site-ccache:\$GUILE_LOAD_COMPILED_PATH\""
echo "  export LD_LIBRARY_PATH=\"$GUILE_PREFIX/lib:\$LD_LIBRARY_PATH\""
echo
echo "Version installed:"
"$GUILE_PREFIX/bin/guile" --version | head -1 || echo "Installation verification failed"

# Create activation script
cat > "$GUILE_PREFIX/activate.sh" << EOF
#!/bin/bash
# Activate Guile main branch environment
export PATH="$GUILE_PREFIX/bin:\$PATH"
export GUILE_LOAD_PATH="$GUILE_PREFIX/share/guile/site/3.0:\$GUILE_LOAD_PATH"
export GUILE_LOAD_COMPILED_PATH="$GUILE_PREFIX/lib/guile/3.0/site-ccache:\$GUILE_LOAD_COMPILED_PATH"
export LD_LIBRARY_PATH="$GUILE_PREFIX/lib:\$LD_LIBRARY_PATH"
echo "Guile main branch environment activated"
guile --version | head -1
EOF
chmod +x "$GUILE_PREFIX/activate.sh"

echo
echo "Quick activation: source $GUILE_PREFIX/activate.sh"