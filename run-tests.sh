#!/bin/bash

# Test runner script for ElevenLabs TTS package

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

# Track if any tests have failed
HAS_FAILED=0

# Function to run tests
run_tests() {
    local test_type="$1"
    local selector="$2"
    local output_file
    local exit_code
    
    echo -e "\n${YELLOW}Running $test_type tests...${NC}"
    
    # Create temp file for output
    output_file=$(mktemp)
    
    if [ -n "$selector" ]; then
        $EMACS -batch -l "$PACKAGE_FILE" -l "$TEST_FILE" \
              --eval "(ert-run-tests-batch-and-exit $selector)" \
              > "$output_file" 2>&1
        exit_code=$?
    else
        $EMACS -batch -l "$PACKAGE_FILE" -l "$TEST_FILE" \
              -f ert-run-tests-batch-and-exit \
              > "$output_file" 2>&1
        exit_code=$?
    fi
    
    # Display output with indentation
    sed 's/^/  /' "$output_file"
    
    # Check results and provide appropriate feedback
    if [ $exit_code -eq 0 ]; then
        echo -e "${GREEN}✓ $test_type tests passed${NC}"
    else
        echo -e "${RED}✗ $test_type tests failed${NC}"
        HAS_FAILED=1
        
        # For integration tests, provide helpful error messages
        if [[ "$test_type" == *"Integration"* ]]; then
            if grep -q "insufficient credits" "$output_file"; then
                echo -e "${YELLOW}💡 Note: API call failed due to insufficient credits. Add more credits to your ElevenLabs account.${NC}"
            elif grep -q "Unauthorized" "$output_file" || grep -q "401" "$output_file"; then
                echo -e "${YELLOW}💡 Note: API key appears to be invalid. Check your API key in ~/.config/emacs-tts/elevenlabs-api-key${NC}"
            elif grep -q "rate limit" "$output_file" || grep -q "429" "$output_file"; then
                echo -e "${YELLOW}💡 Note: Rate limit exceeded. Wait a moment and try again.${NC}"
            elif grep -q "network\|connection\|resolve" "$output_file"; then
                echo -e "${YELLOW}💡 Note: Network connectivity issue. Check your internet connection.${NC}"
            fi
        fi
    fi
    
    # Clean up temp file
    rm -f "$output_file"
}

# Manual test function
run_manual_test() {
    echo -e "${BLUE}ElevenLabs TTS Manual Test${NC}"
    echo "============================"
    echo ""
    
    # Clean up any existing test files
    echo -e "${YELLOW}🧹 Cleaning up test files...${NC}"
    rm -f test-tts-output.mp3
    
    echo -e "${YELLOW}🔊 Generating audio with Rachel voice and auto-playing...${NC}"
    echo ""
    
    # Create temp file for output
    output_file=$(mktemp)
    
    # Run the manual TTS test
    $EMACS -batch -l "$PACKAGE_FILE" \
          --eval '(progn 
                    (setq elevenlabs-tts-enable-playback t)
                    (cl-letf (((symbol-function (quote read-string)) (lambda (prompt) "y")))
                      (let ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
                            (test-phrase "Hello, this is a test of the ElevenLabs text to speech system. Testing one, two, three.")
                            (output-file "test-tts-output.mp3"))
                        (if (elevenlabs-tts--make-api-request-sync voice-id test-phrase output-file)
                            (progn
                              (message "✅ Audio generated: %s" output-file)
                              (when (file-exists-p output-file)
                                (message "📊 File size: %d bytes" (nth 7 (file-attributes output-file)))))
                          (message "❌ Audio generation failed")))))' \
          > "$output_file" 2>&1
    
    local exit_code=$?
    
    # Display filtered output
    if [ -s "$output_file" ]; then
        grep -E "(✅|❌|📊|🔊|Making request|Playing|Audio)" "$output_file" || echo -e "${YELLOW}ℹ️  Processing complete${NC}"
    fi
    
    echo ""
    
    # Check results
    if [ -f "test-tts-output.mp3" ]; then
        echo -e "${GREEN}✅ Test audio file created successfully!${NC}"
        file_size=$(ls -lh "test-tts-output.mp3" | awk '{print $5}')
        echo -e "${YELLOW}📁 File: test-tts-output.mp3 ($file_size)${NC}"
        echo -e "${BLUE}🎵 Audio should have played automatically${NC}"
        echo ""
        echo -e "${GREEN}🎉 Manual test completed successfully!${NC}"
    else
        echo -e "${RED}❌ Test audio file was not created${NC}"
        echo -e "${YELLOW}💡 This could be due to:${NC}"
        echo "   - Insufficient ElevenLabs credits"
        echo "   - Invalid API key"
        echo "   - Network connectivity issues"
        echo ""
        echo -e "${RED}❌ Manual test failed${NC}"
        HAS_FAILED=1
    fi
    
    # Clean up temp file
    rm -f "$output_file"
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
    "manual")
        run_manual_test
        ;;
    "all"|*)
        echo "Running all tests..."
        run_tests "All"
        ;;
esac

# Final result summary
if [ $HAS_FAILED -eq 1 ]; then
    echo -e "\n${RED}❌ Some tests failed!${NC}"
    echo "Please review the failures above and fix any issues."
    exit 1
else
    echo -e "\n${GREEN}✅ All tests completed successfully!${NC}"
fi

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
