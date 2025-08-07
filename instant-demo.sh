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
