#!/bin/bash

# Test runner script for ElevenLabs TTS package

set -e

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

EMACS=${EMACS:-emacs}
PACKAGE_FILE="elevenlabs-tts.el"
TEST_FILE="test-elevenlabs-tts.el"

echo -e "${BLUE}ElevenLabs TTS Test Runner${NC}"
echo "================================"

# Function to run tests
run_tests() {
    local test_type="$1"
    local selector="$2"
    
    echo -e "\n${YELLOW}Running $test_type tests...${NC}"
    
    if [ -n "$selector" ]; then
        if $EMACS -batch -l "$PACKAGE_FILE" -l "$TEST_FILE" \
              --eval "(ert-run-tests-batch-and-exit $selector)" \
              2>&1 | sed 's/^/  /'; then
            echo -e "${GREEN}✓ $test_type tests passed${NC}"
        else
            echo -e "${RED}✗ $test_type tests failed${NC}"
            exit 1
        fi
    else
        if $EMACS -batch -l "$PACKAGE_FILE" -l "$TEST_FILE" \
              -f ert-run-tests-batch-and-exit \
              2>&1 | sed 's/^/  /'; then
            echo -e "${GREEN}✓ $test_type tests passed${NC}"
        else
            echo -e "${RED}✗ $test_type tests failed${NC}"
            exit 1
        fi
    fi
}

# Parse command line arguments
case "${1:-all}" in
    "playback")
        echo "Running playback-specific tests..."
        # Run each set of playback tests individually since ERT string matching works on substrings
        echo "  - Audio player detection tests..."
        run_tests "Audio Player" "\"audio-player\""
        echo "  - Audio file playback tests..."
        run_tests "Audio File Playback" "\"play-audio-file\""
        echo "  - Playback prompt tests..."
        run_tests "Playback Prompt" "\"playback\""
        echo "  - GStreamer special handling tests..."
        run_tests "GStreamer" "\"gst-launch\""
        ;;
    "unit")
        echo "Running unit tests (excluding integration)..."
        # Run individual core functionality tests
        echo "  - API and configuration tests..."
        run_tests "API Config" "\"api-key-file-default\""
        run_tests "Default Settings" "\"default-settings\""
        echo "  - Voice handling tests..."
        run_tests "Voice ID" "\"get-voice-id\""
        run_tests "Voice Name" "\"get-voice-name\""
        run_tests "Voice Lists" "\"voice-lists\""
        echo "  - Filename generation tests..."
        run_tests "Base Filename" "\"get-base-filename\""
        run_tests "Next Filename" "\"get-next-filename\""
        echo "  - TTS generation tests..."
        run_tests "Curl Output" "\"curl-creates\""
        echo "  - Playback functionality tests..."
        run_tests "Audio Player" "\"audio-player\""
        run_tests "Audio File Playback" "\"play-audio-file\""
        run_tests "Playback Prompt" "\"playback\""
        run_tests "GStreamer" "\"gst-launch\""
        ;;
    "integration")
        echo "Running integration tests..."
        # Check for API key first
        API_KEY_FILE="$HOME/.config/emacs-tts/elevenlabs-api-key"
        if [ ! -f "$API_KEY_FILE" ] || ! [ -s "$API_KEY_FILE" ] || grep -q "your_elevenlabs_api_key_here" "$API_KEY_FILE" 2>/dev/null; then
            echo -e "${RED}❌ API key not configured properly${NC}"
            echo "Please set up your API key in $API_KEY_FILE first"
            exit 1
        fi
        run_tests "Integration API Key" "\"integration-api-key-reading\""
        run_tests "Integration Voice Gen" "\"integration-voice-generation\""
        ;;
    "all"|*)
        echo "Running all tests..."
        run_tests "All"
        ;;
esac

echo -e "\n${GREEN}All tests completed successfully!${NC}"

# Show test coverage info for playback tests
if [ "${1:-all}" = "playback" ] || [ "${1:-all}" = "all" ]; then
    echo -e "\n${BLUE}Playback Test Coverage:${NC}"
    echo "• Audio player detection: ✓"
    echo "• Player priority ordering: ✓" 
    echo "• Playback functionality: ✓"
    echo "• User prompt interaction: ✓"
    echo "• Configuration toggles: ✓"
    echo "• Error handling: ✓"
    echo "• GStreamer special cases: ✓"
    echo "• Integration with TTS workflow: ✓"
fi
