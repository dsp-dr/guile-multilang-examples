#!/usr/bin/env bash
# Instant recording setup - hold Option to capture last 30 seconds

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_ROOT"

echo "🎬 Instant Recording Setup for Demos"
echo "===================================="
echo ""

# Create the continuous recording script
cat > continuous-record.sh << 'EOF'
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
EOF

chmod +x continuous-record.sh

# Create tmux-based recording with hotkey
cat > tmux-instant-record.sh << 'EOF'
#!/usr/bin/env bash
# Tmux-based instant recording with Option key trigger

OUTPUT_DIR="./recordings"
mkdir -p "$OUTPUT_DIR"

echo "🎬 Tmux Instant Recording"
echo "========================"
echo ""
echo "Setting up tmux session with recording..."

# Kill existing session if it exists
tmux kill-session -t demo-record 2>/dev/null || true

# Start new tmux session in detached mode
tmux new-session -d -s demo-record

# Set up key binding for Option+R to save recording
tmux bind-key -n M-r run-shell "
    tmux capture-pane -t demo-record -p > recordings/capture-\$(date +%Y%m%d-%H%M%S).txt && 
    echo 'Recording saved!'
"

# Start asciinema recording in the tmux session
tmux send-keys -t demo-record "asciinema rec --append recordings/session.cast" Enter
sleep 2

echo "✅ Recording session started!"
echo ""
echo "Commands:"
echo "  • tmux attach -t demo-record    - Attach to session"
echo "  • Press Option+R (Alt+R)        - Save current buffer"
echo "  • tmux kill-session -t demo-record - Stop recording"
echo ""
echo "Your work is being continuously recorded in the background."
echo "Press Option+R at any time to save a snapshot."
EOF

chmod +x tmux-instant-record.sh

# Create OBS Studio alternative script
cat > obs-instant-record.sh << 'EOF'
#!/usr/bin/env bash
# OBS Studio configuration for instant replay buffer

echo "🎬 OBS Studio Instant Replay Setup"
echo "=================================="
echo ""
echo "For GUI-based recording with instant replay:"
echo ""
echo "1. Install OBS Studio:"
echo "   pkg install obs-studio"
echo ""
echo "2. Configure Replay Buffer:"
echo "   • Settings → Output → Replay Buffer"
echo "   • Enable Replay Buffer"
echo "   • Set duration: 30 seconds"
echo "   • Set hotkey: Option+R"
echo ""
echo "3. Start recording:"
echo "   • Click 'Start Replay Buffer'"
echo "   • Work normally"
echo "   • Press Option+R to save last 30 seconds"
echo ""
echo "This provides high-quality video recording with minimal overhead."
EOF

chmod +x obs-instant-record.sh

# Create the main launcher
cat > instant-demo.sh << 'EOF'
#!/usr/bin/env bash
# Main instant demo recording launcher

echo "🎬 Instant Demo Recording Options"
echo "================================="
echo ""
echo "Choose your recording method:"
echo ""
echo "1. Terminal Recording (asciinema)"
echo "   ./continuous-record.sh"
echo "   - Records terminal only"
echo "   - Saves as .cast and .gif"
echo "   - Low overhead"
echo ""
echo "2. Tmux Session Recording"
echo "   ./tmux-instant-record.sh"
echo "   - Records tmux session"
echo "   - Option+R to save buffer"
echo "   - Good for terminal demos"
echo ""
echo "3. OBS Studio (full screen)"
echo "   ./obs-instant-record.sh"
echo "   - Records entire screen"
echo "   - Professional quality"
echo "   - Requires GUI"
echo ""
echo "Which method would you like to use? (1/2/3): "
read -n 1 choice
echo ""

case $choice in
    1)
        ./continuous-record.sh
        ;;
    2)
        ./tmux-instant-record.sh
        ;;
    3)
        ./obs-instant-record.sh
        ;;
    *)
        echo "Invalid choice"
        exit 1
        ;;
esac
EOF

chmod +x instant-demo.sh

echo "✅ Instant recording scripts created!"
echo ""
echo "Available recording methods:"
echo ""
echo "1. 📺 Continuous Recording (./continuous-record.sh)"
echo "   - Runs asciinema in 30-second ring buffer"
echo "   - Press Option/Alt to save last 30 seconds"
echo "   - Automatically converts to GIF"
echo ""
echo "2. 🖥️  Tmux Recording (./tmux-instant-record.sh)"
echo "   - Records tmux session continuously"
echo "   - Option+R saves current buffer"
echo "   - Good for terminal workflows"
echo ""
echo "3. 🎥 OBS Studio Setup (./obs-instant-record.sh)"
echo "   - Instructions for GUI recording"
echo "   - Professional replay buffer"
echo "   - Best quality output"
echo ""
echo "Run ./instant-demo.sh to choose your method!"