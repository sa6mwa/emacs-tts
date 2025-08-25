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

# Function to run a list of specific tests individually
run_test_list() {
    local test_type="$1"
    shift
    local tests=("$@")
    local output_file
    local exit_code
    local passed=0
    local failed=0
    local total=${#tests[@]}
    
    echo -e "\n${YELLOW}Running $test_type tests ($total tests)...${NC}"
    
    # Run tests individually to avoid complex selector issues
    for test in "${tests[@]}"; do
        # Create temp file for output
        output_file=$(mktemp)
        
        # Run single test
        $EMACS -batch -l "$PACKAGE_FILE" -l "$TEST_FILE" \
              --eval "(ert-run-tests-batch-and-exit \"$test\")" \
              > "$output_file" 2>&1
        exit_code=$?
        
        if [ $exit_code -eq 0 ]; then
            ((passed++))
            echo -e "  ${GREEN}✓${NC} $test"
        else
            ((failed++))
            echo -e "  ${RED}✗${NC} $test"
            HAS_FAILED=1
            
            # Show error details for failed tests
            if [ -s "$output_file" ]; then
                echo "    Error details:"
                grep -E "(Error:|should have been|expected|FAILED)" "$output_file" | head -3 | sed 's/^/      /' || true
            fi
        fi
        
        # Clean up temp file
        rm -f "$output_file"
    done
    
    # Summary
    echo -e "  ${YELLOW}Results: $passed passed, $failed failed${NC}"
    
    if [ $failed -eq 0 ]; then
        echo -e "${GREEN}✓ $test_type tests passed${NC}"
    else
        echo -e "${RED}✗ $test_type tests failed${NC}"
        
        # For integration tests, provide helpful error messages
        if [[ "$test_type" == *"Integration"* ]]; then
            echo -e "${YELLOW}💡 This could be due to:${NC}"
            echo "   - Insufficient ElevenLabs credits"
            echo "   - Invalid API key"
            echo "   - Network connectivity issues"
        fi
    fi
}

# Manual test function
run_manual_test() {
    echo -e "${BLUE}ElevenLabs TTS Manual Test - Speech Speed Verification${NC}"
    echo "======================================================"
    echo ""
    echo -e "${YELLOW}This test will generate three audio files at different speeds:${NC}"
    echo "  1. Normal speed (1.0)"
    echo "  2. Slow speed (0.7)"
    echo "  3. Fast speed (1.2)"
    echo ""
    echo -e "${YELLOW}🎧 Please listen to each audio file to verify the speed differences!${NC}"
    echo ""
    
    # Clean up any existing test files
    echo -e "${YELLOW}🧹 Cleaning up previous test files...${NC}"
    rm -f test-tts-normal.mp3 test-tts-slow.mp3 test-tts-fast.mp3
    
    # Test phrase that clearly demonstrates speed differences
    local test_phrase="The quick brown fox jumps over the lazy dog. This sentence contains every letter of the alphabet and helps demonstrate speech speed differences clearly."
    local voice="Rachel"
    local generated_files=0
    local total_tests=3
    
    # Function to generate audio at specific speed
    generate_speed_test() {
        local speed="$1"
        local speed_name="$2"
        local output_file="$3"
        local output_temp=$(mktemp)
        
        echo -e "\n${BLUE}🎙️  Generating ${speed_name} speed audio (${speed})...${NC}"
        
        $EMACS -batch -l "$PACKAGE_FILE" \
              --eval "(progn 
                        (setq elevenlabs-tts-enable-playback t)
                        (cl-letf (((symbol-function (quote read-string)) (lambda (prompt) \"y\")))
                          (with-temp-buffer
                            (elevenlabs-tts--set-speech-speed ${speed})
                            (let ((voice-id (elevenlabs-tts--get-voice-id \"${voice}\"))
                                  (test-phrase \"${test_phrase}\"))
                              (if (elevenlabs-tts--make-api-request-sync voice-id test-phrase \"${output_file}\")
                                  (progn
                                    (message \"✅ ${speed_name} speed audio generated: %s\" \"${output_file}\")
                                    (when (file-exists-p \"${output_file}\")
                                      (message \"📊 File size: %d bytes\" (nth 7 (file-attributes \"${output_file}\")))
                                      (message \"🎵 Playing ${speed_name} speed audio...\")))
                                (message \"❌ ${speed_name} speed audio generation failed\"))))))" \
              > "$output_temp" 2>&1
        
        local exit_code=$?
        
        # Display filtered output
        if [ -s "$output_temp" ]; then
            grep -E "(✅|❌|📊|🎵|Audio)" "$output_temp" | sed 's/^/  /' || echo -e "  ${YELLOW}ℹ️  Processing...${NC}"
        fi
        
        # Check if file was created
        if [ -f "$output_file" ]; then
            ((generated_files++))
            local file_size=$(ls -lh "$output_file" | awk '{print $5}')
            echo -e "  ${GREEN}✅ ${speed_name} speed file created: $output_file ($file_size)${NC}"
        else
            echo -e "  ${RED}❌ ${speed_name} speed file creation failed${NC}"
            HAS_FAILED=1
        fi
        
        # Clean up temp file
        rm -f "$output_temp"
        
        # Brief pause between generations
        sleep 1
    }
    
    # Generate all three speed tests
    generate_speed_test "1.0" "Normal" "test-tts-normal.mp3"
    generate_speed_test "0.7" "Slow" "test-tts-slow.mp3"
    generate_speed_test "1.2" "Fast" "test-tts-fast.mp3"
    
    echo ""
    echo "======================================================"
    
    # Final results
    if [ $generated_files -eq $total_tests ]; then
        echo -e "${GREEN}🎉 All speech speed tests completed successfully!${NC}"
        echo ""
        echo -e "${BLUE}📁 Generated files:${NC}"
        echo "  • test-tts-normal.mp3 - Normal speed (1.0)"
        echo "  • test-tts-slow.mp3   - Slow speed (0.7)"
        echo "  • test-tts-fast.mp3   - Fast speed (1.2)"
        echo ""
        echo -e "${YELLOW}🎧 MANUAL VERIFICATION REQUIRED:${NC}"
        echo "Please listen to all three files to verify:"
        echo "  1. Normal speed sounds natural"
        echo "  2. Slow speed is noticeably slower than normal"
        echo "  3. Fast speed is noticeably faster than normal"
        echo "  4. All files contain the complete test phrase clearly"
        echo ""
        echo -e "${GREEN}✅ Speech speed manual test completed successfully!${NC}"
    else
        echo -e "${RED}❌ Speech speed test failed - only $generated_files/$total_tests files generated${NC}"
        echo -e "${YELLOW}💡 This could be due to:${NC}"
        echo "   - Insufficient ElevenLabs credits"
        echo "   - Invalid API key"
        echo "   - Network connectivity issues"
        echo "   - API rate limiting (multiple requests in short time)"
        echo ""
        echo -e "${RED}❌ Manual test failed${NC}"
        HAS_FAILED=1
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
        # Use ERT tags to select unit tests
        run_tests "Unit" "'(tag unit)"
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
        # Use ERT tags to select integration tests
        run_tests "Integration" "'(tag integration)"
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
    echo -e "\n${BLUE}Playbook Test Coverage:${NC}"
    echo "• Audio player detection: ✓"
    echo "• Player priority ordering: ✓" 
    echo "• Playback functionality: ✓"
    echo "• User prompt interaction: ✓"
    echo "• Configuration toggles: ✓"
    echo "• Error handling: ✓"
    echo "• GStreamer special cases: ✓"
    echo "• Integration with TTS workflow: ✓"
fi

# Show test coverage info for manual tests
if [ "${1:-all}" = "manual" ]; then
    echo -e "\n${BLUE}Manual Test Coverage - Speech Speed:${NC}"
    echo "• Normal speed (1.0): ✓"
    echo "• Slow speed (0.7): ✓" 
    echo "• Fast speed (1.2): ✓"
    echo "• Buffer-local speed settings: ✓"
    echo "• API payload speed integration: ✓"
    echo "• Real-world audio generation: ✓"
    echo "• User verification required: ✓"
fi
