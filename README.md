# Emacs ElevenLabs Text-to-Speech

An Emacs package that integrates with ElevenLabs' text-to-speech API to convert selected text into high-quality audio files.

## Features

- Convert selected text to speech using ElevenLabs' premium voices
- Support for both male and female voices
- Sequential file naming (automatically generates `filename-01.mp3`, `filename-02.mp3`, etc.)
- Audio files saved in the same directory as the current buffer
- Interactive voice selection or quick shortcuts
- Convenient keybindings

## Installation

### Prerequisites

1. **ElevenLabs API Key**: Sign up at [ElevenLabs.io](https://elevenlabs.io/) and get your API key
2. **Emacs 25.1 or later**

### Setup Instructions

1. **Clone this repository** to your Emacs configuration directory:
   ```bash
   cd ~/.emacs.d/
   git clone https://github.com/sa6mwa/emacs-tts.git
   ```

2. **Set up your API key**:
   ```bash
   # Create config directory
   mkdir -p ~/.config/emacs-tts
   
   # Create API key file with your actual key
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
   - `M-x elevenlabs-tts-speak-selection` - Interactive voice selection
   - `M-x elevenlabs-tts-speak-selection-quick` - Quick with first voice of chosen gender

### Keybindings (if enabled)

| Key Binding | Function |
|-------------|----------|
| `C-c s` | Interactive voice selection |
| `C-c S` | Quick male voice (first in list) |
| `C-c M-s` | Quick female voice (first in list) |

### File Naming

Audio files are saved with sequential naming:
- First file: `buffer-name-01.mp3`
- Second file: `buffer-name-02.mp3`
- And so on...

Files are saved in the same directory as the current buffer, or in the current working directory if the buffer has no associated file.

## Examples

### Example: Complete Workflow
1. Open a text file: `/home/user/documents/article.txt`
2. Select some text: "Hello, this is a test of the text-to-speech system."
3. Press `C-c s`
4. Choose voice from completion list (e.g., Josh, Rachel, etc.)
5. Confirm file path: `/home/user/documents/article-01.mp3` (or edit as needed)
6. Wait for generation
7. See success message: "✅ Audio successfully saved to: /home/user/documents/article-01.mp3"

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

### Testing Your Setup

Create a simple test:

1. Create a new buffer: `C-x b test-tts RET`
2. Type some text: "Hello, this is a test of the text to speech system."
3. Select the text: `C-x h`
4. Press: `C-c s` and choose any voice
5. Check for `test-tts-01.mp3` in your current directory

## API Costs

ElevenLabs charges per character processed. Check your account usage at [ElevenLabs.io](https://elevenlabs.io/). The package uses the `eleven_monolingual_v1` model for optimal English speech.

## Contributing

Feel free to submit issues and pull requests to improve this package.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

