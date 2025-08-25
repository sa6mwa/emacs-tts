# Emacs ElevenLabs Text-to-Speech

An Emacs package that integrates with ElevenLabs' text-to-speech API to convert selected text into high-quality audio files.

## Features

- Convert selected text to speech using ElevenLabs' premium voices
- Support for both male and female voices
- Comprehensive error handling with user-friendly messages for common issues
- Automatic MP3 file validation and corrupted file detection
- Optional audio playback with support for multiple players (ffplay, mplayer, mpv, vlc, etc.)
- Per-buffer output directory: Each buffer can have its own custom output directory
- Smart filename generation with voice names (e.g., `article-0001-rachel.mp3`, `article-0002-josh.mp3`)
- Global sequential numbering across all voices (0001, 0002, 0003...)
- Supports up to 9,999 files with 4-digit numbering
- Flexible output location: custom directory per buffer or default to buffer's directory
- Interactive voice selection or quick shortcuts
- Convenient keybindings
- Extensive test suite with both unit and integration tests

## Installation

### Prerequisites

1. **ElevenLabs API Key**: Sign up at [ElevenLabs.io](https://elevenlabs.io/) and get your API key
2. **Emacs 25.1 or later**
3. **curl** (recommended): For better UTF-8 text handling. The package automatically falls back to Emacs' built-in HTTP client if curl is not available.
4. **Audio Player** (optional): For automatic playback functionality - supports `ffplay`, `mplayer`, `mpg123`, `mpg321`, `mpv`, `vlc`, and GStreamer

### Setup Instructions

1. **Clone this repository** and install:
   ```bash
   git clone https://github.com/sa6mwa/emacs-tts.git
   cd emacs-tts
   make install
   ```

   This will:
   - Set up the API key configuration directory
   - Install the package to `~/.emacs.d/emacs-tts`
   - Display setup instructions

2. **Add your API key** to the created file:
   ```bash
   # Add your actual API key to the config file
   echo "your_actual_api_key_here" > ~/.config/emacs-tts/elevenlabs-api-key
   ```

3. **Add to your Emacs configuration** (`.emacs` or `init.el`):
   ```elisp
   ;; Add the package to load path
   (add-to-list 'load-path "~/.emacs.d/emacs-tts")
   
   ;; Load the package
   (require 'elevenlabs-tts)
   
   ;; Set up the main keybinding C-c s
   (elevenlabs-tts-setup-keybinding)
   
   ;; OR: Set up custom keybindings (override default C-c s)
   ;; (global-set-key (kbd "C-c t") 'elevenlabs-tts-speak-selection)
   ;; (global-set-key (kbd "C-c T") (lambda () (interactive) (elevenlabs-tts-speak-selection-quick 'male)))
   ```

4. **Restart Emacs** or evaluate the configuration.

## Configuration

### API Key Setup

The package reads your API key from `~/.config/emacs-tts/elevenlabs-api-key`. Create this file with your API key on the first line:

```bash
# Method 1: Using echo
echo "your_actual_api_key_here" > ~/.config/emacs-tts/elevenlabs-api-key

# Method 2: Using a text editor
vim ~/.config/emacs-tts/elevenlabs-api-key
# Add your API key on the first line, save and exit
```

**Important**: Keep this file private and secure! You can customize the API key file location:

```elisp
(setq elevenlabs-tts-api-key-file "/path/to/your/api-key-file")
```

### Voice Configuration

The package comes with pre-configured voices including:

**Male voices:** Drew, Clyde, Antoni, Josh, Arnold, Adam, Sam, David, Ethan, Brian, Callum, Charlie, George, Liam

**Female voices:** Rachel, Domi, Bella, Elli, Emily, Matilda, Grace, Serena, Dorothy, Charlotte

You can customize the voice lists:

```elisp
;; Add or modify voices
(setq elevenlabs-tts-male-voices 
      '("Josh" "Adam" "Sam"))  ; Your preferred male voices first

(setq elevenlabs-tts-female-voices 
      '("Rachel" "Bella" "Emily"))  ; Your preferred female voices first
```

### Voice Settings

Customize the voice parameters:

```elisp
(setq elevenlabs-tts-default-settings
      '((stability . 0.7)        ; 0.0-1.0, higher = more stable
        (similarity_boost . 0.6) ; 0.0-1.0, higher = more similar to original
        (style . 0.2)            ; 0.0-1.0, style exaggeration
        (use_speaker_boost . t))) ; boolean, enhance clarity
```

## Usage

### Basic Usage

1. **Select text** in any Emacs buffer
2. **Run one of the commands**:
   - `M-x elevenlabs-tts-speak-selection` - Interactive voice selection with output directory prompt
   - `M-x elevenlabs-tts-speak-selection-quick` - Quick with first voice of chosen gender (uses current buffer's output directory)

### Output Directory Management

**Per-buffer output directories**: Each buffer maintains its own output directory setting independently.

#### Commands
- `M-x elevenlabs-tts-set-output-directory` - Set or view the output directory for the current buffer

#### How it works

1. **First time usage**: When you run `elevenlabs-tts-speak-selection`, you'll be prompted to choose an output directory
2. **Subsequent usage**: The selected directory is remembered for that specific buffer
3. **Default behavior**: If no custom directory is set, files are saved to the same directory as the buffer's file
4. **Reset to default**: Enter an empty string when prompted to reset to the default directory

#### Examples

```
Buffer A (~/documents/article.txt):
- Custom output: ~/audio/articles/
- Files: ~/audio/articles/article-0001-rachel.mp3

Buffer B (~/code/readme.md):
- Custom output: ~/project-audio/
- Files: ~/project-audio/readme-0001-josh.mp3

Buffer C (~/notes.txt):
- Default output (same as buffer): ~/
- Files: ~/notes-0001-bella.mp3
```

Each buffer's output directory setting persists until you change it or close the buffer.

### Keybindings (if enabled)

| Key Binding | Function |
|-------------|----------|
| `C-c s` | Interactive voice selection |
| `C-c S` | Quick male voice (first in list) |
| `C-c M-s` | Quick female voice (first in list) |

### File Naming

Audio files are saved with smart sequential naming that includes voice information:

**Format:** `basename-####-voicename.mp3`

**Examples:**
- `article-0001-rachel.mp3` ← First file with Rachel
- `article-0002-josh.mp3` ← Second file with Josh  
- `article-0003-rachel.mp3` ← Third file with Rachel again
- `article-0004-charlie.mp3` ← Fourth file with Charlie

**Features:**
- **Voice identification**: Easy to see which voice was used
- **Global numbering**: Sequence continues across all voices (no per-voice restart)
- **4-digit format**: Supports up to 9,999 files with proper sorting
- **Chronological order**: Numbers reflect creation order regardless of voice changes

Files are saved in the same directory as the current buffer, or in the current working directory if the buffer has no associated file.

## Examples

### Example: Complete Workflow
1. Open a text file: `/home/user/documents/article.txt`
2. Select some text: "Hello, this is a test of the text-to-speech system."
3. Press `C-c s`
4. **Choose output directory**: First time, you'll be prompted for output directory (e.g., accept default `/home/user/documents/` or choose custom like `/home/user/audio/`)
5. **Choose voice** from completion list (e.g., Josh, Rachel, etc.)
6. **Confirm file path**: `/home/user/documents/article-0001-josh.mp3` (or edit as needed)
7. Wait for generation
8. See success message: "✅ Audio successfully saved to: /home/user/documents/article-0001-josh.mp3"
9. Next selection with Rachel creates: `article-0002-rachel.mp3` (continues sequence in same directory)

## Troubleshooting

### Common Issues

1. **"API key file not found"**
   - Create the API key file: `~/.config/emacs-tts/elevenlabs-api-key`
   - Make sure your API key is on the first line

2. **"Voice ID not found"**
   - Check that the voice name is spelled correctly in the voice lists
   - Verify the voice ID is correct (ElevenLabs may update voice IDs)

3. **"❌ API Error" or "❌ HTTP Error"**
   - Check your internet connection
   - Verify your API key is valid and has remaining credits
   - Check ElevenLabs API status at https://status.elevenlabs.io/

4. **File not saved**
   - Check write permissions in the target directory
   - Ensure the directory exists

5. **Unicode/Multibyte Text Issues**
   - The package automatically handles UTF-8 encoding for special characters, accents, smart quotes, etc.
   - If you encounter "Multibyte text in HTTP request" errors, ensure curl is installed
   - Enable debug mode for detailed diagnostics: `(setq elevenlabs-tts-debug t)`

### Debug Mode

Enable debug output to troubleshoot API issues:

```elisp
;; Method 1: Toggle debug mode interactively
M-x elevenlabs-tts-toggle-debug

;; Method 2: Set debug mode programmatically
(setq elevenlabs-tts-debug t)   ; Enable
(setq elevenlabs-tts-debug nil) ; Disable (default)
```

With debug mode enabled, you'll see detailed information including:
- JSON payload being sent to the API
- Temporary file locations
- Curl exit codes and full output
- HTTP response codes and error messages

### Testing Your Setup

#### Manual Test
The package includes a built-in manual test:

```bash
# Quick manual test (requires API key and credits)
make test-manual
```

#### Interactive Test
Create a simple test manually:

1. Create a new buffer: `C-x b test-tts RET`
2. Type some text: "Hello, this is a test of the text to speech system."
3. Select the text: `C-x h`
4. Press: `C-c s` and choose any voice (e.g., Rachel)
5. Check for `test-tts-0001-rachel.mp3` in your current directory

#### Comprehensive Testing
Run the full test suite:

```bash
# Run all tests (unit + integration)
make test

# Run only unit tests (no API key required)
make test-unit

# Run only integration tests (requires API key and credits)
make test-integration

# Test audio playback functionality
make test-playback
```

## Security Features

The package includes several security improvements:

- **API Key Protection**: API keys are never exposed in process lists (invisible to `ps aux`)
- **Shell Injection Prevention**: Filenames are properly escaped to prevent shell injection attacks
- **Secure Temporary Files**: Uses curl config files instead of command-line arguments for sensitive data
- **Automatic Cleanup**: All temporary files are securely deleted after use

## API Costs

ElevenLabs charges per character processed. Check your account usage at [ElevenLabs.io](https://elevenlabs.io/). The package uses the `eleven_monolingual_v1` model for optimal English speech.

## Changelog

For detailed information about recent improvements including comprehensive error handling, audio playback integration, and extensive test suite enhancements, see [CHANGELOG.md](CHANGELOG.md).

## Contributing

Feel free to submit issues and pull requests to improve this package.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

