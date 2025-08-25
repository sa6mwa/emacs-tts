# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **Comprehensive Error Handling System**
  - Intelligent API error detection and parsing from ElevenLabs responses
  - User-friendly error messages for common issues (insufficient credits, invalid API key, rate limiting)
  - HTTP status code extraction and interpretation from curl responses
  - Automatic MP3 file validation using magic bytes detection
  - Corrupted file cleanup when API returns invalid responses

- **Audio Playback Integration**
  - Optional automatic playback after TTS generation
  - Support for multiple audio players: `ffplay`, `mplayer`, `mpg123`, `mpg321`, `mpv`, `vlc`, GStreamer
  - Intelligent audio player detection with priority ordering
  - Special handling for GStreamer with proper file:// URI formatting
  - User prompt for playback confirmation with configurable defaults
  - `elevenlabs-tts-toggle-playback` command to enable/disable playback
  - `elevenlabs-tts-test-audio-players` command to test available players

- **Comprehensive Test Suite**
  - **Unit Tests**: 31 tests covering all core functionality with complete API mocking
  - **Integration Tests**: Real API tests that properly fail on API errors
  - **Error Handling Tests**: Validation of all error scenarios and edge cases
  - **Playback Tests**: Audio player detection, priority, and functionality tests
  - **MP3 Validation Tests**: File format verification and corruption detection
  - Manual test functionality via `make test-manual`

- **Improved Build System**
  - Enhanced Makefile with better test organization
  - Improved `run-tests.sh` script with manual test support
  - Better error handling in test runner (removed `set -e` for comprehensive reporting)
  - Color-coded test output with emoji indicators
  - Detailed test coverage reporting

### Enhanced
- **API Request Functions**
  - Added comprehensive error checking to `elevenlabs-tts--make-curl-request`
  - Added fallback error handling to `elevenlabs-tts--make-url-request`
  - Improved HTTP status code detection from curl output
  - Better JSON error parsing from API responses
  - Automatic cleanup of invalid/corrupted output files

- **File Validation**
  - `elevenlabs-tts--is-valid-mp3-file`: Validates MP3 files using magic bytes
  - Support for both ID3v2 headers and MP3 frame sync detection
  - Minimum file size validation (>10 bytes)
  - Binary-safe file content analysis

- **Error Message Formatting**
  - `elevenlabs-tts--format-api-error-message`: User-friendly error descriptions
  - Contextual suggestions for resolving common issues
  - Specific handling for HTTP status codes (401, 402, 429, 500, etc.)

- **Test Infrastructure**
  - Proper separation of unit tests (fully mocked) and integration tests (real API)
  - Unit tests no longer require API keys or network access
  - Integration tests properly fail when API returns errors
  - Comprehensive mocking of curl, file operations, and user interactions

### Fixed
- **Syntax Errors**
  - Fixed unbalanced parentheses in main source file
  - Resolved "end-of-file" errors during package loading
  - Corrected MP3 validation test file size requirements

- **Test Suite Issues**
  - Fixed MP3 validation tests to create files with sufficient size (>10 bytes)
  - Corrected binary data creation in test helper functions
  - Fixed test script error handling to show all failures instead of stopping at first error
  - Improved manual test output formatting and eliminated weird bash scripting issues

- **File Handling**
  - Ensured proper cleanup of invalid files when API returns errors
  - Better binary file handling in tests and validation functions

### Technical Improvements
- **Code Organization**
  - Better separation of concerns between Makefile and bash scripts
  - Cleaner test function organization and naming
  - Improved error handling patterns throughout codebase

- **Documentation**
  - Added comprehensive function documentation for all new error handling functions
  - Improved test function descriptions and categorization
  - Better code comments explaining complex validation logic

### Testing
- **New Test Categories**:
  - `test-elevenlabs-tts-unit-api-request-*`: Complete API mocking tests
  - `test-elevenlabs-tts-*-error-handling-*`: Error scenario validation
  - `test-elevenlabs-tts-is-valid-mp3-file-*`: File validation tests
  - Audio playback functionality tests
  - Integration tests with proper failure handling

- **Test Commands**:
  - `make test-unit`: Run unit tests only (no API key required)
  - `make test-integration`: Run integration tests (requires API key and credits)
  - `make test-playback`: Test audio player functionality
  - `make test-manual`: Generate and play test audio file
  - `make test`: Run comprehensive test suite

### Changed
- Integration tests now properly **fail** when API returns errors instead of silently skipping
- Manual test moved from complex Makefile inline scripting to clean bash function
- Test runner provides complete failure reporting instead of stopping at first error
- MP3 file validation now requires minimum file size to prevent false positives

### Security
- All API mocking in unit tests uses dummy keys to prevent accidental real API calls
- Improved file validation prevents processing of potentially malicious non-MP3 files
- Better temporary file cleanup in error scenarios

---

## Previous Releases
- See git history for changes prior to this changelog

