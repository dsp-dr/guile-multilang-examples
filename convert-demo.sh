#!/usr/bin/env bash
# Convert asciinema recording to various formats

set -e

CAST_FILE="guile-multilang-demo.cast"

if [[ ! -f "$CAST_FILE" ]]; then
    echo "❌ Recording file not found: $CAST_FILE"
    exit 1
fi

echo "🎬 Converting asciinema recording: $CAST_FILE"
echo "============================================"

# Check available conversion tools
echo "📋 Available conversion options:"
echo ""

# Check for agg (best for GIF conversion)
if command -v agg >/dev/null 2>&1; then
    echo "✅ agg - Converting to GIF..."
    agg "$CAST_FILE" guile-multilang-demo.gif
    echo "   → guile-multilang-demo.gif"
else
    echo "❌ agg not found (install: npm install -g @asciinema/agg)"
fi

# Check for svg-term
if command -v svg-term >/dev/null 2>&1; then
    echo "✅ svg-term - Converting to SVG..."
    svg-term --in "$CAST_FILE" --out guile-multilang-demo.svg --window
    echo "   → guile-multilang-demo.svg"
else
    echo "❌ svg-term not found (install: npm install -g svg-term-cli)"
fi

# Check for asciinema2gif (Python-based)
if command -v asciinema2gif >/dev/null 2>&1; then
    echo "✅ asciinema2gif - Converting to GIF..."
    asciinema2gif "$CAST_FILE" guile-multilang-demo-alt.gif
    echo "   → guile-multilang-demo-alt.gif"
else
    echo "❌ asciinema2gif not found (install: pip install asciinema2gif)"
fi

echo ""
echo "🔧 Installation commands for missing tools:"
echo "  npm install -g @asciinema/agg"
echo "  npm install -g svg-term-cli" 
echo "  pip install asciinema2gif"
echo ""

# Show file sizes
echo "📊 Output files:"
ls -lh guile-multilang-demo.* 2>/dev/null || echo "No output files generated yet"
echo ""

echo "✅ Conversion script complete!"
echo "Run with conversion tools installed to generate GIF/SVG output"