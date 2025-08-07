#!/usr/bin/env bash
# Continuous recording with ring buffer for instant capture

RECORD_DIR="${HOME}/.demo-recordings"
BUFFER_FILE="${RECORD_DIR}/buffer.cast"
OUTPUT_DIR="./recordings"

mkdir -p "$RECORD_DIR"
mkdir -p "$OUTPUT_DIR"

echo "🔴 Starting continuous recording..."
echo "Hold Option key to save last 30 seconds"
echo "Press Ctrl+C to stop"
echo ""

# Start asciinema in append mode with overwrite
while true; do
    # Record in 30-second segments, overwriting old buffer
    timeout 30 asciinema rec \
        --quiet \
        --overwrite \
        --title "Demo Buffer" \
        "${BUFFER_FILE}" 2>/dev/null || true
done &

RECORD_PID=$!

# Function to save buffer on Option key
save_buffer() {
    TIMESTAMP=$(date +%Y%m%d-%H%M%S)
    OUTPUT_FILE="${OUTPUT_DIR}/demo-${TIMESTAMP}.cast"
    
    if [[ -f "$BUFFER_FILE" ]]; then
        cp "$BUFFER_FILE" "$OUTPUT_FILE"
        echo "✅ Saved recording: $OUTPUT_FILE"
        
        # Convert to GIF if agg is available
        if command -v agg >/dev/null 2>&1; then
            echo "Converting to GIF..."
            agg "$OUTPUT_FILE" "${OUTPUT_DIR}/demo-${TIMESTAMP}.gif"
            echo "✅ GIF created: ${OUTPUT_DIR}/demo-${TIMESTAMP}.gif"
        fi
    else
        echo "❌ No buffer available yet"
    fi
}

# Monitor for Option key (Alt key on Linux/BSD)
echo "Monitoring for Option/Alt key..."
echo "Instructions:"
echo "  • Hold Option/Alt to save last 30 seconds"
echo "  • Press Ctrl+C to stop recording"
echo ""

# Trap cleanup
cleanup() {
    echo ""
    echo "Stopping continuous recording..."
    kill $RECORD_PID 2>/dev/null || true
    exit 0
}
trap cleanup EXIT INT TERM

# Simple key monitoring (platform-specific)
if [[ "$OSTYPE" == "freebsd"* ]]; then
    # FreeBSD key monitoring
    while true; do
        read -n 1 -s key
        if [[ "$key" == $'\033' ]]; then
            save_buffer
        fi
    done
else
    # Linux fallback
    while true; do
        read -n 1 -s key
        if [[ "$key" == $'\033' ]]; then
            save_buffer
        fi
    done
fi
